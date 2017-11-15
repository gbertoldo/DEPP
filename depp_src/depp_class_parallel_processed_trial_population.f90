
!> \brief Provides a class for parallel processing a trial population.

module mod_class_parallel_processed_trial_population

   use mod_mpi
   use mod_class_abstract_parallel_processed_data
   use mod_class_DE_RSM
   use mod_class_system_variables
   use mod_class_ehist
   use mod_class_abstract_search_strategy
   use mod_search_strategy_factory
   use tools
   use output

   implicit none

   ! Makes everything private, except otherwise stated
   private

   !> \brief Provides a class for parallel processing a trial population.
   type, extends(class_abstract_parallel_processed_data), public :: class_parallel_processed_trial_population

      class(class_system_variables),         pointer :: sys_var
      class(class_ehist),                    pointer :: ehist
      class(class_abstract_search_strategy), pointer :: searcher => null()
      real(8), allocatable                           :: xfit(:)    ! Fitness of the trial individual
      real(8), dimension(:,:), allocatable           :: x          ! Trial individual

   contains


      procedure, public, pass :: init
      procedure, public, pass :: get_trial_population


      ! Procedures deferred from parent class
      procedure, private, pass :: data_size
      procedure, private, pass :: compute
      procedure, private, pass :: send
      procedure, private, pass :: recv
      procedure, private, pass :: update

   end type

contains


   !> \brief Constructor
   subroutine init(this, sys_var, ehist)
      implicit none
      class(class_parallel_processed_trial_population)  :: this
      class(class_system_variables),         target, intent(in) :: sys_var
      class(class_ehist),                    target, intent(in) :: ehist

      integer           :: estatus   ! Exit status (0=success; 1=failure)


      this%sys_var  => sys_var
      this%ehist    => ehist

      ! Allocating resources
      allocate(this%x(ehist%np,ehist%nu))
      allocate(this%xfit(ehist%np))


      ! Initializing search strategy object

      !call create_search_strategy(sys_var,      "DE-RSM",   this%searcher)
      allocate(class_DE_RSM::this%searcher)


      associate ( searcher => this%searcher)

         ! Initializing the object
         select type (searcher)

   !         type is ( class_DE_RAND_1 )
   !
   !            call ind_gen%init()

            type is ( class_DE_RSM )

               call searcher%init(sys_var,ehist%np)

         end select

      end associate

   end subroutine


   !> \brief To be removed
   subroutine get_trial_population(this, x, xfit)
      implicit none
      class(class_parallel_processed_trial_population)  :: this
      real(8),                              intent(out) :: xfit(:)    ! Fitness of the trial individual
      real(8), dimension(:,:),              intent(out) :: x          ! Trial individual


      call this%compute_concurrent()

      call this%update()

      xfit = this%xfit
      x    = this%x

   end subroutine


   !> User defined: Gives the size of the array of data.
   integer function data_size(this)
      implicit none
      class(class_parallel_processed_trial_population) :: this

      data_size = this%ehist%np

   end function


   !> User defined. Tells MPI how to send each element of data from current thread to 'to_thread'.
   subroutine send(this, i, to_thread)
      implicit none
      class(class_parallel_processed_trial_population) :: this
      integer,                    intent(in) :: i
      integer,                    intent(in) :: to_thread

      call mpi_send(    this%x(i,:), this%ehist%nu, mpi_double_precision, to_thread, mpio%tag, mpio%comm, mpio%code)
      call mpi_send(   this%xfit(i),  1, mpi_double_precision, to_thread, mpio%tag, mpio%comm, mpio%code)

      call this%searcher%send(i, to_thread)

   end subroutine


   !> User defined. Tells MPI how to receive each element of data from 'from_thread' to current thread.
   subroutine recv(this, i, from_thread)
      implicit none
      class(class_parallel_processed_trial_population) :: this
      integer,                    intent(in) :: i
      integer,                    intent(in) :: from_thread

      call mpi_recv(    this%x(i,:), this%ehist%nu, mpi_double_precision, from_thread, mpio%tag, mpio%comm, mpio%status, mpio%code)
      call mpi_recv(   this%xfit(i),  1, mpi_double_precision, from_thread, mpio%tag, mpio%comm, mpio%status, mpio%code)

      call this%searcher%recv(i, from_thread)

   end subroutine



   subroutine update(this)
      implicit none
      class(class_parallel_processed_trial_population) :: this

      call this%searcher%update()

   end subroutine


   !> User defined. Defines the procedure for calculating each data element.
   subroutine compute(this,i)
      implicit none
      class(class_parallel_processed_trial_population) :: this
      integer, intent(in) :: i

      ! Inner variables
      integer :: ind
      integer :: estatus


      ! Creating labels
      associate (sys_var  => this%sys_var, &
                 ehist    => this%ehist,   &
                 searcher => this%searcher,&
                 xfit     => this%xfit,    &
                 x        => this%x        )

         ind = i


         ! Calculating the fitness function
         fitloop: do


            ! Calculating a trial individual
            call searcher%get_trial(ind, ehist, x(ind,:), estatus)


            ! Checking the constraints
            if ( is_X_out_of_range(ehist%nu, ehist%xmin, ehist%xmax, x(ind,:)) ) then

               call sys_var%logger%print("Searcher must provide trial individuals that satisfy constraints. Stopping.")

               call mod_mpi_finalize()

            end if


            ! Asking to the external program 'ffit' the fitness of individual 'x'
            call get_fitness(sys_var, ehist%sname, ind, ehist%nu, x(ind,:), ehist%xname, &
               xfit(ind), estatus)

            if (ehist%g==1 .and. estatus==1) estatus=2


            call searcher%feed_back(ind, ehist, xfit(ind), estatus)


            ! Analyzing the exit status of the external program
            select case (estatus)

               case (0) ! Success

                  exit fitloop

               case (1) ! Failure

                  ! Failure in the calculation of fitness function. Saving informations.
                  call save_fitness_failure(ehist%nu, ehist%g, ind, sys_var, ehist%sname, &
                     x(ind,:), estatus)

                  x(ind,:) = ehist%pop(ind,:)

                  xfit(ind) = ehist%fit(ind)

                  exit fitloop

               case (2:10) ! Generate another individual

                  ! Failure in the calculation of fitness function. Saving informations.
                  call save_fitness_failure(ehist%nu, ehist%g, ind, sys_var, ehist%sname, &
                     x(ind,:), estatus)

               case default

                  call sys_var%logger%print("ERROR: external program returned an unknown status. Stopping...")

                  call mod_mpi_finalize()

            end select

         end do fitloop

      end associate

   end subroutine

end module
