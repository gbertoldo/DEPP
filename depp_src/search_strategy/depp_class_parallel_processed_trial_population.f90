
!> \brief Provides a class for parallel processing a trial population.

module mod_class_parallel_processed_trial_population

   use mod_mpi
   use mod_class_abstract_parallel_processed_data
   use mod_class_system_variables
   use mod_class_ehist
   use mod_class_abstract_search_strategy
   use mod_class_search_strategy_factory
   use mod_class_abstract_fitness_calculator
   use mod_class_fitness_calculator_factory
   use mod_class_fitness_calculator_exit_code
   use mod_class_ifile
   use mod_search_tools
   use mod_string

   implicit none


   ! Makes everything private, except otherwise stated
   private


   !> \brief Provides a class for parallel processing a trial population.
   type, extends(class_abstract_parallel_processed_data), public :: class_parallel_processed_trial_population

      class(class_system_variables),            pointer :: sys_var        => null()
      class(class_ehist),                       pointer :: ehist          => null()
      class(class_abstract_search_strategy),    pointer :: searcher       => null()
      class(class_abstract_fitness_calculator), pointer :: fit_calculator => null()

      ! These vector of data are calculated separately by each thread and shared after
      ! all parallel computation is finished.
      real(8), allocatable, dimension(:)    :: trial_fit !< Fitness of the trial population
      real(8), allocatable, dimension(:,:)  :: trial_pop !< Trial population

   contains


      procedure, public, pass :: init
      procedure, public, pass :: get_trial_population


      ! Procedures deferred from parent class and implemented here
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
      class(class_system_variables), target, intent(in) :: sys_var
      class(class_ehist),            target, intent(in) :: ehist

      ! Inner variables
      type(class_ifile)                      :: ifile
      type(class_fitness_calculator_factory) :: fit_calculator_factory
      type(class_search_strategy_factory)    :: search_strategy_factory
      character(len=str_size)                :: search_strategy_conf


      ! Associating pointers
      this%sys_var  => sys_var
      this%ehist    => ehist


      ! Allocating resources
      allocate(this%trial_pop(ehist%np,ehist%nu))
      allocate(this%trial_fit(ehist%np))


      ! Creating the search strategy object
      call ifile%init( filename=this%sys_var%absparfile, field_separator="&" )
      call ifile%load()
      call ifile%get_value(search_strategy_conf,  "search_strategy_conf")

      search_strategy_conf = trim(sys_var%absfolderin) // trim(search_strategy_conf)

      call search_strategy_factory%create(sys_var, search_strategy_conf, this%searcher)


      ! Since the individual searcher is run in parallel, it may contains data that need
      ! to be shared among threads after all parallel computation is finished. This data
      ! sharing is made through send/receive/update methods implemented in the searcher
      ! module. The size of the shared data vector in the searcher module must be equal
      ! to the size of the shared data vector of the current module.
      if ( this%searcher%data_size() /= this%data_size() ) then

         call this%sys_var%logger%print("")
         call this%sys_var%logger%print("The size of the data vector in the population searcher " &
         // "and the size of the data vector of the individual searcher must be equal. Stopping.")
         call this%sys_var%logger%print("")

         call mod_mpi_finalize()

      end if


      ! Creating a fitness calculator object
      call fit_calculator_factory%create(sys_var, ehist, "EXTERNAL_CALCULATOR", this%fit_calculator)


      ! Since the fitness calculator is run in parallel, it may contains data that need
      ! to be shared among threads after all parallel computation is finished. This data
      ! sharing is made through send/receive/update methods implemented in the fitness
      ! calculator module. The size of the data shared vector in the fitness calculator module
      ! must be equal to the size of the shared data vector of the current module.
      if ( this%fit_calculator%data_size() /= this%data_size() ) then

         call this%sys_var%logger%print("")
         call this%sys_var%logger%print("The size of the data vector in the population searcher " &
         // "and the size of the data vector of the fitness calculator module must be equal. Stopping.")
         call this%sys_var%logger%print("")

         call mod_mpi_finalize()

      end if


   end subroutine


   !> \brief Computes the trial population and the corresponding fitness function
   subroutine get_trial_population(this)
      implicit none
      class(class_parallel_processed_trial_population)  :: this


      ! Compute the trial individuals and their fitness in parallel
      call this%compute_concurrent()


      ! Update data
      call this%update()


      ! Print statistics of fitness calculation into logger
      call this%sys_var%logger%print(this%fit_calculator%statistics_info())


      ! Processors synchronization
      call mod_mpi_barrier()


      ! Adds the trial population, calculated by searcher, to the evolution history
      call this%ehist%add_trial_population(this%trial_pop, this%trial_fit)

   end subroutine


   !> User defined: Gives the size of the array of data to be parallel computed.
   integer function data_size(this)
      implicit none
      class(class_parallel_processed_trial_population) :: this

      data_size = this%ehist%np

   end function


   !> User defined. Defines the procedure for calculating each data element.
   subroutine compute(this,i)
      implicit none
      class(class_parallel_processed_trial_population) :: this
      integer, intent(in)                              :: i

      ! Inner variables
      integer :: ecode


      ! Creating labels
      associate (sys_var        =>    this%sys_var,        &
                 ehist          =>    this%ehist,          &
                 searcher       =>    this%searcher,       &
                 fit_calculator =>    this%fit_calculator, &
                 trial_fit      =>    this%trial_fit,      &
                 trial_pop      =>    this%trial_pop       )


         ! Calculating the fitness function
         fitloop: do


            ! Calculating a trial individual
            call searcher%get_trial(i, ehist, trial_pop(i,:))


            ! Checking the constraints
            if ( is_X_out_of_range(ehist%nu, ehist%xmin, ehist%xmax, trial_pop(i,:)) ) then

               call sys_var%logger%print("Searcher must provide trial individuals that satisfy constraints. Stopping.")

               call mod_mpi_finalize()

            end if


            ! Asking the fitness calculator the fitness of the trial individual
            call fit_calculator%get_fitness(ehist, i, trial_pop(i,:), trial_fit(i), ecode)


            ! Sending feedback to searcher
            call searcher%feed_back(i, ehist, trial_fit(i), ecode)


            ! Analyzing the exit code of the fitness calculator
            select case (ecode)


               ! In case of success, just exit the loop
               case (fitness_calculator_exit_code%SUCCESS)

                  exit fitloop


               ! Fitness calculation fail and no other individual will be generated
               case (fitness_calculator_exit_code%FAIL_AND_GIVE_UP)

                  trial_pop(i,:) = ehist%pop(i,:)

                  trial_fit(i)   = ehist%fit(i)

                  exit fitloop


               ! Fitness calculation fail and another individual will be generated
               case (fitness_calculator_exit_code%FAIL_AND_TRY_AGAIN)


               ! Unknown cases
               case default

                  call sys_var%logger%print("ERROR: fitness calculator returned an unknown status. Stopping...")

                  call mod_mpi_finalize()


            end select


         end do fitloop


      end associate


   end subroutine


   !> User defined. Tells MPI how to send each element of data from current thread to 'to_thread'.
   subroutine send(this, i, to_thread)
      implicit none
      class(class_parallel_processed_trial_population) :: this
      integer,                              intent(in) :: i
      integer,                              intent(in) :: to_thread

      call mod_mpi_send(to_thread, this%trial_pop(i,:))

      call mod_mpi_send(to_thread, this%trial_fit(i))

      call this%searcher%send(i, to_thread)

      call this%fit_calculator%send(i, to_thread)

   end subroutine


   !> User defined. Tells MPI how to receive each element of data from 'from_thread' to current thread.
   subroutine recv(this, i, from_thread)
      implicit none
      class(class_parallel_processed_trial_population) :: this
      integer,                              intent(in) :: i
      integer,                              intent(in) :: from_thread

      call mod_mpi_recv(from_thread, this%trial_pop(i,:))

      call mod_mpi_recv(from_thread, this%trial_fit(i))

      call this%searcher%recv(i, from_thread)

      call this%fit_calculator%recv(i, from_thread)

   end subroutine


   !> \brief Perform update actions after all parallel computation have been finished.
   subroutine update(this)
      implicit none
      class(class_parallel_processed_trial_population) :: this

      call this%searcher%update()

      call this%fit_calculator%update()

   end subroutine


end module
