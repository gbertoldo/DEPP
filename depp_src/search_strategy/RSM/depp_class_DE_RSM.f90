
!> \brief Defines the search strategy DE-RSM

module mod_class_DE_RSM

   use mod_class_abstract_search_strategy_factory
   use mod_class_abstract_search_strategy
   use mod_random_generator
   use mod_class_system_variables
   use mod_mpi
   use mod_class_ehist
   use mod_class_RSM_search_strategy
   use mod_search_tools
   use mod_class_ifile
   use mod_string
   use mod_class_DE_RSM_hybridization_control

   implicit none

   ! Makes everything private, except otherwise stated
   private

   ! Public class defining DE-RSM search strategy
   type, public, extends(class_abstract_search_strategy) :: class_DE_RSM

      private

      ! These data are calculated separately by each thread and must be exchanged
      ! before the next generation.
      integer, allocatable                           :: rsm_tag(:)   ! Stores the return state of application of DE-RSM
      real(8), allocatable                           :: curnt_fit(:) ! Fitness of the current individuals
      real(8), allocatable                           :: trial_fit(:) ! Fitness of the trial individuals

      ! Hybridization control
      type(class_DE_RSM_hybridization_control)       :: hybrid_control

      ! Search strategies
      class(class_abstract_search_strategy), pointer :: de_searcher  => null()
      class(class_abstract_search_strategy), pointer :: rsm_searcher => null()


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
      integer             :: estatus            ! Exit status
      type(class_ifile)   :: ifile1             ! Input file
      type(class_ifile)   :: ifile2             ! Input file
      character(str_size) :: de_conf_file_name  ! DE configuration file
      character(str_size) :: rsm_conf_file_name ! RSM configuration file
      character(str_size) :: CID                ! Class ID
      integer             :: np                 ! Population size
      real(8)             :: fh                 ! Hybridization fraction/Initial hybridization fraction
      real(8)             :: fhmin              ! Minimum hybridization factor
      real(8)             :: fhmax              ! Maximum hybridization factor
      integer             :: fhm                ! Model for the dynamical calculation of the factor of hybridization
      integer             :: nf                 ! Number of fitting points for response surface adjustment


      call ifile1%init(filename=sys_var%absparfile, field_separator='&')
      call ifile2%init(filename=conf_file_name,     field_separator='&')

      call ifile1%load()
      call ifile2%load()

      call ifile1%get_value(        np,    "np")
      call ifile2%get_value(       CID,   "CID")
      call ifile2%get_value(        fh,    "fh")
      call ifile2%get_value(     fhmin, "fhmin")
      call ifile2%get_value(     fhmax, "fhmax")
      call ifile2%get_value(       fhm,   "fhm")

      if (trim(CID)/="DE-RSM") then

         call sys_var%logger%print("class_DE_RSM: unexpected CID. Stopping.")

         call mod_mpi_finalize()

      end if


      ! Creating DE search strategy

      call ifile2%get_value(de_conf_file_name,"de_search_strategy_conf")

      de_conf_file_name = trim(sys_var%absfolderin) // trim(de_conf_file_name)

      call search_strategy_factory%create(sys_var, de_conf_file_name, this%de_searcher)


      ! Creating RSM search strategy

      call ifile2%get_value(rsm_conf_file_name,"rsm_search_strategy_conf")

      rsm_conf_file_name = trim(sys_var%absfolderin) // trim(rsm_conf_file_name)

      call search_strategy_factory%create(sys_var, rsm_conf_file_name, this%rsm_searcher)


      ! Getting nf
      associate (rsm_searcher => this%rsm_searcher)

         select type (rsm_searcher)

            type is ( class_RSM_search_strategy )

               nf = rsm_searcher%get_nf()

         end select

      end associate


      ! Allocating resources

      allocate(this%rsm_tag(np)  )
      allocate(this%trial_fit(np))
      allocate(this%curnt_fit(np))

      ! Initializing the hybridization control object
      call this%hybrid_control%init(sys_var, np, nf, fh, fhmin, fhmax, fhm)

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

      call mod_mpi_send(to_thread, this%rsm_tag(i)  )
      call mod_mpi_send(to_thread, this%trial_fit(i))
      call mod_mpi_send(to_thread, this%curnt_fit(i))

   end subroutine


   !> User defined. Tells MPI how to receive each element of data from 'from_thread' to current thread.
   subroutine recv(this, i, from_thread)
      implicit none
      class(class_DE_RSM) :: this
      integer,                    intent(in) :: i
      integer,                    intent(in) :: from_thread

      call mod_mpi_recv(from_thread, this%rsm_tag(i)  )
      call mod_mpi_recv(from_thread, this%trial_fit(i))
      call mod_mpi_recv(from_thread, this%curnt_fit(i))

   end subroutine


   !> Performs update tasks before the next generation.
   subroutine update(this)
      implicit none
      class(class_DE_RSM) :: this

      ! Inner variables
      integer :: i

      do i = 1, size(this%rsm_tag)

         call this%hybrid_control%add(this%rsm_tag(i), this%trial_fit(i), this%curnt_fit(i))

      end do

      call this%hybrid_control%update()

   end subroutine



   !> \brief Generates a trial individual
   subroutine get_trial(this, ind, ehist, x, es)
      implicit none
      class(class_DE_RSM)                   :: this
      integer,                  intent(in)  :: ind   ! Number of the individual of the population
      class(class_ehist),       intent(in)  :: ehist ! Evolution history
      real(8), dimension(:),    intent(out) :: x     ! Trial individual
      integer, optional,        intent(out) :: es    ! Exit status


      integer :: estatus
      integer :: rsmstatus

      if (present(es)) es = 0

      if (ehist%g==1) then

         ! Creating the trial individual x
         estatus = DE_RSM_RETURN%DE_APPLIED

         call get_random_individual(ehist%nu, ehist%xmin, ehist%xmax, x)

      else

         ! Checking if RSM can be applied

         if ( this%hybrid_control%is_rsm_applicable(ehist%g) ) then

            ! rsm_tag stores the return state of application of DE-RSM
            estatus = DE_RSM_RETURN%RSM_APPLIED

            ! Generating a RSM individual

            call this%rsm_searcher%get_trial(ind, ehist, x, rsmstatus)

            ! If RSM fails, generates a pure DE individual
            if ( rsmstatus == 1 ) then

               ! rsm_tag stores the return state of application of DE-RSM
               estatus = DE_RSM_RETURN%DE_APPLIED

               ! Creating the trial individual x
               call this%de_searcher%get_trial(ind, ehist, x)

            end if

         else

            ! rsm_tag stores the return state of application of DE-RSM
            estatus = DE_RSM_RETURN%DE_APPLIED

            call this%de_searcher%get_trial(ind, ehist, x)

            estatus = DE_RSM_RETURN%DE_APPLIED

         end if

      end if


      ! Verifying the constraints. If the individual x is out of range,
      ! another one is created using pure DE
      do while ( is_X_out_of_range(ehist%nu, ehist%xmin, ehist%xmax, x) )

         ! Creating the trial individual x
         call this%de_searcher%get_trial(ind, ehist, x)

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
