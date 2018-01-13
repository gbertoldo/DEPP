
!> \brief This module contains the optimization algorithm

module mod_class_optimizer

   use mod_mpi
   use mod_random_generator
   use mod_class_btimer
   use mod_class_system_variables
   use mod_class_ehist
   use mod_class_composite_stop_condition
   use mod_class_parallel_processed_trial_population

   implicit none

   ! Makes everything private, except otherwise stated
   private

   !> \brief Class optimizer
   type, public :: class_optimizer

      private

      type(class_system_variables)                    :: sys_var  !< System variables
      type(class_ehist)                               :: ehist    !< Evolution history
      type(class_btimer)                              :: timer    !< Timer
      type(class_composite_stop_condition)            :: stopper  !< Stop condition object
      type(class_parallel_processed_trial_population) :: searcher !< Searcher object

   contains

      procedure, public, pass :: init !< Constructor
      procedure, public, pass :: run  !< Performs optimization

   end type


contains


   !> \brief Constructor
   subroutine init(this)
      implicit none
      class(class_optimizer) :: this !< A reference to this object


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
         !call initialize_random_generator()


         ! Initializing system variables
         call sys_var%init()


         ! Initializing evolution history
         call ehist%init(sys_var)


         ! Initializing trial individuals searcher
         call searcher%init(sys_var, ehist)


         ! Initializing stop condition object
         call stopper%init(sys_var)


         ! Initializing timer
         call timer%init(sys_var)


      end associate


   end subroutine



   !> \brief Optimization algorithm
   subroutine run(this)
      implicit none
      class(class_optimizer) :: this !< A reference to this object


      ! Creating labels
      associate (                      &
            sys_var  => this%sys_var,  &
            timer    => this%timer,    &
            ehist    => this%ehist,    &
            searcher => this%searcher, &
            stopper  => this%stopper   )



         ! Processors synchronization
         call mod_mpi_barrier()


         ! Starting the generations loop. This loop is maintained while the stopping
         ! condition is not satisfied
         do

            ! Computing stop condition
            call stopper%compute_stop_condition(ehist)


            ! Printing convergence measure of the current generation
            call sys_var%logger%print(stopper%convergence_info())


            ! Checking stop condition
            if ( stopper%is_stop_condition_satisfied() ) exit


            ! Starting a new generation
            call ehist%new_generation()


            ! Printing CPU time
            call timer%measure()

            call sys_var%logger%print("Accumulated CPU time: " // timer%formatted_elapsed_time())


            ! Generates a trial population and calculates its fitness function
            ! based on the evolution history (ehist). The trial population is added
            ! to the evolution history.
            call searcher%get_trial_population()


            ! Selects best individuals among trial population and current population
            call ehist%select_individuals()


            ! Only for master
            if (mpio%master) then

               call sys_var%logger%print(ehist%info())

               ! Calculating the ellapsed CPU time
               call timer%measure()

               ! Saving backup of cpu time
               call timer%save_backup()

               ! Saving backup data
               call ehist%save_backup(sys_var)

            end if

            call mod_mpi_barrier()

         end do


         ! Printing final solution
         call sys_var%logger%print(ehist%final_solution_info())
         call sys_var%logger%print(stopper%convergence_info())
         call sys_var%logger%print(timer%formatted_elapsed_time() // " : Accumulated CPU time")


         ! Finishing MPI
         call mod_mpi_finalize()


      end associate


   end subroutine


end module
