
!> \brief This module contains the optimization algorithm

module mod_class_optimizer

   use mod_mpi
   use mod_random_generator
   use mod_class_btimer
   use mod_class_system_variables
   use mod_class_ehist
   use mod_class_composite_stop_condition
   use mod_class_parallel_processed_trial_population
   use output

   implicit none

   ! Makes everything private, except otherwise stated
   private

   !> \brief Class optimizer
   type, public :: class_optimizer

      private

      type(class_system_variables)                    :: sys_var            ! System variables
      type(class_ehist)                               :: ehist              ! Evolution history
      type(class_btimer)                              :: timer              ! Timer
      type(class_composite_stop_condition)            :: stopper            ! Stop condition object
      type(class_parallel_processed_trial_population) :: searcher           ! Searcher object

   contains

      procedure, public, pass :: init
      procedure, public, pass :: run

   end type


contains


   !> \brief Constructor
   subroutine init(this)
      implicit none
      class(class_optimizer) :: this


      ! Creating labels
      associate (                      &
            sys_var  => this%sys_var,  &
            timer    => this%timer,    &
            ehist    => this%ehist,    &
            searcher => this%searcher, &
            stopper  => this%stopper   )


         ! Initializing MPI module
         call mod_mpi_init()


         ! Initializing random number generator module
         !call initialize_random_generator(mpi%iproc)


         ! Initializing system variables
         call sys_var%init()


         ! Initializing evolution history
         call ehist%init(sys_var)


         ! Initializing trial individuals searcher
         call searcher%init(sys_var, ehist)


         ! Initializing stop condition object
         call stopper%init(sys_var)


! TODO (guilherme#1#): Find a more appropriate way of writing output
         ! if iproc == 0, master processor writes the parameter to a file
         if (mpi%iproc == 0) then

            ! Writting parameters to the output file
            call write_parameters(sys_var, ehist%sname, 0)

         end if


         ! Initializing timer
         call timer%init(sys_var)


      end associate


   end subroutine



   !> \brief Optimization algorithm
   subroutine run(this)
      implicit none
      class(class_optimizer) :: this

      ! Inner variables
      integer                              :: i       ! Dummy variable
      real(8), allocatable                 :: xfit(:) ! Fitness of the trial individual
      real(8), dimension(:,:), allocatable :: x       ! Trial individual


      ! Creating labels
      associate (                      &
            sys_var  => this%sys_var,  &
            timer    => this%timer,    &
            ehist    => this%ehist,    &
            searcher => this%searcher, &
            stopper  => this%stopper   )



         ! Allocating resources
         allocate(x(ehist%np,ehist%nu))
         allocate(xfit(ehist%np))


         call mod_mpi_barrier()


         ! Starting the generations loop. This loop is maintained while the stopping
         ! condition is not satisfied
         do

            call stopper%compute_stop_condition(ehist)

            ! Printing convergence measure of the current generation
            if (mpi%iproc==0) then

               write(*,*) stopper%convergence_info()

            end if

            if ( stopper%is_stop_condition_satisfied() ) exit


            ! Starting a new generation
            ehist%g = ehist%g+1


            ! Print time
            if (mpi%iproc == 0) then

               call timer%measure()

               write(*,"(/, a, a)")        "Accumulated CPU time: ", timer%formatted_elapsed_time()
               write(*,"(/, a, i4, a, /)") "Processing the", ehist%g, "th generation..."

            end if

            ! Generates a trial population and calculates its fitness function
            ! based on the evolution history (ehist)
            call searcher%get_trial_population(x, xfit)


            call mod_mpi_barrier()


            ! For each individual of the population
            do i = 1, ehist%np

               ! Updating history
               ehist%hist(ehist%g,i,1:ehist%nu) = x(i,:)  ! Individual
               ehist%hist(ehist%g,i,         0) = xfit(i) ! Fitness of the individual

               ! Selecting the best individual
               if ( xfit(i) >= ehist%fit(i)) then

                  ehist%pop(i,:) = x(i,:)

                  ehist%fit(i) = xfit(i)

                  if ( xfit(i) >= ehist%fit(ehist%ibest)) ehist%ibest = i

               end if

            end do


            if (mpi%iproc == 0) then

               ! For each individual of the population
               do i = 1, ehist%np

                  write(*,"(a, i4, a, 10(1pe23.15, 2x))") "The performance of the",  i, "th individual is ", xfit(i), x(i,:)
                  write(21,"(2(i12), 100(2x, 1pe23.15))") ehist%g, i, xfit(i), x(i,:)

                  call flush(21)

               end do

               write(20,"(i12, 3(2x, 1pe23.15),A)") ehist%g, sum(ehist%fit)/ehist%np, maxval(ehist%fit)

               call flush(20)

               ! Calculating the ellapsed CPU time
               call timer%measure()

               ! Saving backup of cpu time
               call timer%save_backup()

               ! Saving backup data
               call ehist%save_backup(sys_var)

            end if

            call mod_mpi_barrier()

         end do


         ! Master processor: data post processing
         if (mpi%iproc == 0) then

            ! Measuring cpu time
            call timer%measure()

            ! Writing data to output file
            call write_output_files(sys_var, ehist%sname, ehist%nu, ehist%np, ehist%ibest, ehist%g, timer, &
               stopper%convergence_info(), ehist%xmin, ehist%xmax, ehist%fit, ehist%pop)

         end if


         ! Finishing MPI
         call mod_mpi_finalize()


      end associate


   end subroutine


end module
