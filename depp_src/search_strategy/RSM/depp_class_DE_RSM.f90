
!> \brief Defines the search strategy DE-RSM

module mod_class_DE_RSM

   use mod_class_abstract_search_strategy_factory
   use mod_class_abstract_search_strategy
   use mod_random_generator
   use mod_class_system_variables
   use mod_mpi
   use mod_class_ehist
   use hybrid
   use mod_search_tools
   use mod_class_ifile
   use mod_global_parameters

   implicit none

   ! Makes everything private, except otherwise stated
   private

   ! Public class defining DE-RSM search strategy
   type, public, extends(class_abstract_search_strategy) :: class_DE_RSM

      private
      real(8) :: fh

      ! These data are calculated separately by each thread and must be exchanged
      ! before the next generation.
      integer, allocatable                           :: rsm_tag(:)   ! Stores the return state of application of DE-RSM
      real(8), allocatable                           :: curnt_fit(:) ! Fitness of the current individuals
      real(8), allocatable                           :: trial_fit(:) ! Fitness of the trial individuals


      class(class_abstract_search_strategy), pointer :: searcher => null()


   contains

      procedure, public, pass :: init
      procedure, public, pass :: get_trial
      procedure, public, pass :: feed_back

      procedure, public, pass :: data_size
      procedure, public, pass :: send
      procedure, public, pass :: recv
      procedure, public, pass :: update

   end type

contains

   !> \brief Constructor
   subroutine init(this, sys_var, conf_file_name, search_strategy_factory)
      implicit none
      class(class_DE_RSM)                                       :: this
      class(class_system_variables),                 intent(in) :: sys_var
      character(len=*),                              intent(in) :: conf_file_name
      class(class_abstract_search_strategy_factory), intent(in) :: search_strategy_factory

      ! Inner variables
      integer             :: estatus
      type(class_ifile)   :: ifile1
      type(class_ifile)   :: ifile2
      character(str_size) :: de_conf_file_name
      character(str_size) :: CID
      integer             :: np
      real(8)             :: fh


      call ifile1%init(filename=sys_var%absparfile, field_separator='&')
      call ifile2%init(filename=conf_file_name,     field_separator='&')

      call ifile1%load()
      call ifile2%load()

      call ifile1%get_value( np,  "np")
      call ifile2%get_value( fh,  "fh")
      call ifile2%get_value(CID, "CID")

      if (trim(CID)/="DE-RSM") then

         call sys_var%logger%print("class_DE_RSM: unexpected CID. Stopping.")

         call mod_mpi_finalize()

      end if

      ! Creating DE search strategy

      call ifile2%get_value(de_conf_file_name,"search_strategy_conf")

      de_conf_file_name = trim(sys_var%absfolderin) // trim(de_conf_file_name)

      call search_strategy_factory%create(sys_var, de_conf_file_name, this%searcher)


      ! Initializes hybrid module and checks hybridization necessary condition for RSM

      call initialize_hybrid_module(sys_var, conf_file_name, estatus)

      ! Checking the exit status of the module initialization
      if ( estatus == 1 ) then
         ! Finishing MPI
         call mod_mpi_abort()
      end if

      allocate(this%rsm_tag(np))
      allocate(this%trial_fit(np))
      allocate(this%curnt_fit(np))


   end subroutine



   !> User defined. Returns the size of the number of elements of the data vector that
   !! must be shared among threads.
   integer function data_size(this)
      implicit none
      class(class_DE_RSM) :: this

      data_size = size(this%rsm_tag)

   end function


   !> User defined. Tells MPI how to send each element of data from current thread to 'to_thread'.
   subroutine send(this, i, to_thread)
      implicit none
      class(class_DE_RSM) :: this
      integer,                    intent(in) :: i
      integer,                    intent(in) :: to_thread

      call mod_mpi_send(to_thread, this%rsm_tag(i))
      call mod_mpi_send(to_thread, this%trial_fit(i))
      call mod_mpi_send(to_thread, this%curnt_fit(i))

   end subroutine


   !> User defined. Tells MPI how to receive each element of data from 'from_thread' to current thread.
   subroutine recv(this, i, from_thread)
      implicit none
      class(class_DE_RSM) :: this
      integer,                    intent(in) :: i
      integer,                    intent(in) :: from_thread

      call mod_mpi_recv(from_thread, this%rsm_tag(i))
      call mod_mpi_recv(from_thread, this%trial_fit(i))
      call mod_mpi_recv(from_thread, this%curnt_fit(i))

   end subroutine


   subroutine update(this)
      implicit none
      class(class_DE_RSM) :: this

      ! Inner variables
      integer :: i

      do i = 1, size(this%rsm_tag)

         call add_to_rsm_dynamic_control(this%rsm_tag(i), this%trial_fit(i), this%curnt_fit(i))

      end do

      ! Calculating the hybridization factor
      this%fh = get_hybridization_factor()

   end subroutine



   !> \brief Generates a trial individual
   subroutine get_trial(this, ind, ehist, x)
      implicit none
      class(class_DE_RSM)                   :: this
      integer,                  intent(in)  :: ind   ! Number of the individual of the population
      class(class_ehist),       intent(in)  :: ehist ! Evolution history
      real(8), dimension(:),    intent(out) :: x     ! Trial individual


      integer :: estatus
      integer :: rsmstatus


      if (ehist%g==1) then

         ! Creating the trial individual x
         estatus = DE_RSM_RETURN%DE_APPLIED

         call get_random_individual(ehist%nu, ehist%xmin, ehist%xmax, x)

      else

         ! Checking if RSM can be applied

         if ( rsm_check(ehist%np, ehist%g, this%fh) ) then

            ! rsm_tag stores the return state of application of DE-RSM
            estatus = DE_RSM_RETURN%RSM_APPLIED

            ! Generating a RSM individual

            call get_rsm_optimum(ind, ehist%nu, ehist%np, ehist%ng, ehist%g, ehist%xmin,&
                   ehist%xmax, ehist%pop, ehist%hist, x, rsmstatus)

            ! If RSM fails, generates a pure DE individual
            if ( rsmstatus == 1 ) then

               ! rsm_tag stores the return state of application of DE-RSM
               estatus = DE_RSM_RETURN%DE_APPLIED

               ! Creating the trial individual x
               call this%searcher%get_trial(ind, ehist, x)

               estatus = DE_RSM_RETURN%DE_APPLIED

            end if

         else

            ! rsm_tag stores the return state of application of DE-RSM
            estatus = DE_RSM_RETURN%DE_APPLIED

            call this%searcher%get_trial(ind, ehist, x)

            estatus = DE_RSM_RETURN%DE_APPLIED

         end if

      end if


      ! Verifying the constraints. If the individual x is out of range,
      ! another one is created using pure DE
      do while ( is_X_out_of_range(ehist%nu, ehist%xmin, ehist%xmax, x) )

         ! Creating the trial individual x
         call this%searcher%get_trial(ind, ehist, x)

         estatus = DE_RSM_RETURN%DE_APPLIED

      end do

      this%rsm_tag(ind) = estatus


   end subroutine


   subroutine feed_back(this, ind, ehist, fit, ecode)
      implicit none
      class(class_DE_RSM)                   :: this
      integer,                  intent(in)  :: ind     ! Number of the individual of the population
      class(class_ehist),       intent(in)  :: ehist   ! Evolution history
      real(8),                  intent(in)  :: fit     ! Fitness of the trial individual
      integer,                  intent(in)  :: ecode   ! Error code

      this%curnt_fit(ind) = ehist%fit(ind)
      this%trial_fit(ind) = fit

   end subroutine


end module
