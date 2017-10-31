
!> \brief Provides a class for parallel processing a trial population.

module mod_class_parallel_processed_trial_population

   use mod_mpi
   use mod_class_abstract_parallel_processed_data
   use mod_class_system_variables
   use mod_class_ehist
   use mod_class_abstract_search_strategy
   use mod_search_strategy_factory

   use hybrid
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
      integer, allocatable                           :: rsm_tag(:) ! Stores the return state of application of DE-RSM

      ! To be removed
      real(8) :: fh


   contains


      procedure, public, pass :: init
      procedure, public, pass :: get_trial_population
      procedure, public, pass :: update


      ! Procedures deferred from parent class
      procedure, private, pass :: data_size
      procedure, private, pass :: compute
      procedure, private, pass :: send
      procedure, private, pass :: recv

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
      allocate(this%rsm_tag(ehist%np))

      ! Initializes hybrid module and checks hybridization necessary condition for RSM
      call initialize_hybrid_module(sys_var,estatus)
      ! Checking the exit status of the module initialization
      if ( estatus == 1 ) then
         ! Finishing MPI
         call mod_mpi_abort()
      end if

      ! To be fixed
      this%fh = 0.35d0

      ! Initializing search strategy object
      call create_search_strategy("DE/RAND/1", this%searcher)


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
      call mpi_send(this%rsm_tag(i),  1,          mpi_integer, to_thread, mpio%tag, mpio%comm, mpio%code)

   end subroutine


   !> User defined. Tells MPI how to receive each element of data from 'from_thread' to current thread.
   subroutine recv(this, i, from_thread)
      implicit none
      class(class_parallel_processed_trial_population) :: this
      integer,                    intent(in) :: i
      integer,                    intent(in) :: from_thread

      call mpi_recv(    this%x(i,:), this%ehist%nu, mpi_double_precision, from_thread, mpio%tag, mpio%comm, mpio%status, mpio%code)
      call mpi_recv(   this%xfit(i),  1, mpi_double_precision, from_thread, mpio%tag, mpio%comm, mpio%status, mpio%code)
      call mpi_recv(this%rsm_tag(i),  1,          mpi_integer, from_thread, mpio%tag, mpio%comm, mpio%status, mpio%code)

   end subroutine



   subroutine update(this)
      implicit none
      class(class_parallel_processed_trial_population) :: this

      call this%exchange()

      call update_rsm_dynamic_control(this%rsm_tag, this%xfit, this%ehist%fit)

      ! Calculating the hybridization factor
      this%fh = get_hybridization_factor()

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
                 rsm_tag  => this%rsm_tag, &
                 xfit     => this%xfit,    &
                 x        => this%x,       &
                 fh       => this%fh       )

         ind = i


         ! Calculating the fitness function
         fitloop: do


            if (ehist%g==1) then

               ! Creating the trial individual x
               rsm_tag(ind) = DE_RSM_RETURN%DE_APPLIED

               call get_random_individual(ehist%nu, ehist%xmin, ehist%xmax, x(ind,:))

            else

               ! Checking if RSM can be applied

               if ( rsm_check(ehist%np, ehist%g, fh) ) then

                  ! rsm_tag stores the return state of application of DE-RSM
                  rsm_tag(ind) = DE_RSM_RETURN%RSM_APPLIED

                  ! Generating a RSM individual

                  call get_rsm_optimum(ind, ehist%nu, ehist%np, ehist%ng, ehist%g, ehist%xmin,&
                   ehist%xmax, ehist%pop, ehist%hist, x(ind,:), estatus)


                  ! If RSM fails, generates a pure DE individual
                  if ( estatus == 1 ) then

                     ! rsm_tag stores the return state of application of DE-RSM
                     rsm_tag(ind) = DE_RSM_RETURN%DE_APPLIED_AFTER_RSM_FAILURE

                     ! Creating the trial individual x
                     call searcher%get_trial(ind, ehist%pop, x(ind,:))

                  end if

               else

                  ! rsm_tag stores the return state of application of DE-RSM
                  rsm_tag(ind) = DE_RSM_RETURN%DE_APPLIED

                  call searcher%get_trial(ind, ehist%pop, x(ind,:))

               end if

            end if


            ! Verifying the constraints. If the individual x is out of range,
            ! another one is created using pure DE
            do while ( is_X_out_of_range(ehist%nu, ehist%xmin, ehist%xmax, x(ind,:)) )

               ! Checking DE-RSM status
               select case (rsm_tag(ind))

                  ! If DE was applied, do nothing.
                  case (DE_RSM_RETURN%DE_APPLIED)

                  ! If RSM was applied, a DE individual will be generated. Counts this application as a RSM failure.
                  case (DE_RSM_RETURN%RSM_APPLIED)

                     rsm_tag(ind) = DE_RSM_RETURN%DE_APPLIED_AFTER_RSM_FAILURE

                  ! If DE was applied after a RSM failure, counts this application as a RSM failure.
                  case (DE_RSM_RETURN%DE_APPLIED_AFTER_RSM_FAILURE)

                  ! If black box evaluation failed, do nothing.
                  case (DE_RSM_RETURN%BLACK_BOX_EVALUATION_FAILURE)

                  case default

               end select


               ! Creating the trial individual x
               call searcher%get_trial(ind, ehist%pop, x(ind,:))

            end do


            ! Asking to the external program 'ffit' the fitness of individual 'x'
            call get_fitness(sys_var, ehist%sname, ind, ehist%nu, x(ind,:), ehist%xname, &
               xfit(ind), estatus)

            if (ehist%g==1 .and. estatus==1) estatus=2

            ! Analyzing the exit status of the external program
            select case (estatus)

               case (0) ! Success

                  exit fitloop

               case (1) ! Failure

                  ! rsm_tag stores the return state of application of DE-RSM
                  rsm_tag(ind) = DE_RSM_RETURN%BLACK_BOX_EVALUATION_FAILURE

                  ! Failure in the calculation of fitness function. Saving informations.
                  call save_fitness_failure(ehist%nu, ehist%g, ind, sys_var, ehist%sname, &
                     x(ind,:), estatus)

                  x(ind,:) = ehist%pop(ind,:)

                  xfit(ind) = ehist%fit(ind)

                  exit fitloop

               case (2:10) ! Generate another individual

                  ! rsm_tag stores the return state of application of DE-RSM
                  rsm_tag(ind) = DE_RSM_RETURN%BLACK_BOX_EVALUATION_FAILURE

                  ! Failure in the calculation of fitness function. Saving informations.
                  call save_fitness_failure(ehist%nu, ehist%g, ind, sys_var, ehist%sname, &
                     x(ind,:), estatus)

               case default

                  write(*,*) "ERROR: external program returned an unknown status. Stopping..."

                  stop

            end select

         end do fitloop

      end associate

   end subroutine

end module
