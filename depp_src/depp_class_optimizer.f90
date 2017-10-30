
!> \brief This module contains the optimization algorithm

module mod_class_optimizer

   use mod_global_parameters
   use mod_class_ifile
   use mod_class_system_variables
   use mod_class_ehist
   use hybrid
   use output
   use mod_mpi
   use mod_class_btimer
   use mod_random_generator
   use mod_class_abstract_search_strategy
   use mod_search_strategy_factory
   use mod_class_composite_stop_condition
   use mod_class_parallel_processed_trial_population

   implicit none

   ! Makes everything private, except otherwise stated
   private

   ! Evolution history class
   type, public :: class_optimizer

      private

      type(class_system_variables)                    :: sys_var            ! System variables
      type(class_ehist)                               :: ehist              ! Evolution history
      type(class_btimer)                              :: timer              ! Timer
      class(class_abstract_search_strategy), pointer  :: searcher => null() ! Search strategy object
      type(class_composite_stop_condition)            :: stopper            ! Stop condition object
      type(class_parallel_processed_trial_population) :: trial_pop

   contains

      procedure, public, pass :: init
      procedure, public, pass :: run

   end type


contains

   !> Constructor
   subroutine init(this)
      implicit none
      class(class_optimizer) :: this

      ! Inner variables
      integer           :: estatus   ! Exit status (0=success; 1=failure)
      integer           :: reload    ! Upload backup data
      type(class_ifile) :: ifile     ! Input file reader


      ! Creating labels
      associate (sys_var  => this%sys_var, &
                 timer    => this%timer,   &
                 ehist    => this%ehist,   &
                 searcher => this%searcher,&
                 stopper  => this%stopper  )


      ! Initializing MPI module
      call mod_mpi_init()


      ! Initializing random number generator module
      !call initialize_random_generator(mpi%iproc)


      ! Initializing system variables
      call sys_var%init()


      ! Checking reload option
      call ifile%init(filename=sys_var%absparfile, field_separator="&")
      call ifile%load()
      call ifile%get_value(reload, "reload")


      ! Initializing evolution history
      call ehist%init(sys_var)


      ! Initializing search strategy object
      call create_search_strategy("DE/RAND/1", searcher)
      ! Initializes hybrid module and checks hybridization necessary condition for RSM
      call initialize_hybrid_module(sys_var,estatus)
      ! Checking the exit status of the module initialization
      if ( estatus == 1 ) then
         ! Finishing MPI
         call mod_mpi_abort()
      end if


      ! Trial population parallel processing
      call this%trial_pop%init(sys_var, ehist, searcher)


      ! Initializing stop condition object
      call stopper%init(sys_var)


      ! if iproc == 0, master processor writes the parameter to a file
      if (mpi%iproc == 0) then

         ! Writting parameters to the output file
         call write_parameters(sys_var, ehist%sname, reload)

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

      integer                            :: i       ! Dummy variable
      integer                            :: iaux    ! Dummy variable
      integer                            :: ind     ! Number of the individual
      integer                            :: estatus ! Exit status (0=success; 1=failure)
      real(8), allocatable               :: xfit(:)    ! Fitness of the trial individual
      real(8), dimension(:,:), allocatable :: x       ! Trial individual


      ! To be removed...
      real(8) :: fh =2       !< Fraction of hybridization
      integer, allocatable :: rsm_tag(:)   !< Stores the return state of application of DE-RSM
      integer :: to_thread, from_thread


      ! Creating labels
      associate (sys_var  => this%sys_var, &
                 timer    => this%timer,   &
                 ehist    => this%ehist,   &
                 searcher => this%searcher,&
                 stopper  => this%stopper  )



      ! Allocating resources
      allocate(x(ehist%np,ehist%nu))
      allocate(xfit(ehist%np))
      allocate(rsm_tag(ehist%np))


      !
      call mod_mpi_barrier()


      ! Starting the generations loop. This loop is maintained while the stopping
      ! condition is not satisfied
      do

         call stopper%compute_stop_condition(ehist)

         ! Printing convergence measure of the current generation
         if (mpi%iproc==0) then

            !write(*,*)  ehist%g, trim(convergence_info)
            !write(24,*) ehist%g, trim(convergence_info)
            !call flush(24)

            write(*,*) stopper%convergence_info()

         end if

         if ( stopper%is_stop_condition_satisfied() ) exit


         ! Starting a new generation

         ehist%g = ehist%g+1

         ! Print time
         if (mpi%iproc == 0) then

            call timer%measure()

            write(*,"(/, a, a)") "Accumulated CPU time: ", timer%formatted_elapsed_time()

            write(*,"(/, a, i4, a, /)") "Processing the", ehist%g, "th generation..."

         end if


         call this%trial_pop%set_fh(fh)

         call this%trial_pop%compute_concurrent()

         call this%trial_pop%exchange()

         call this%trial_pop%get_x_f_rsm(x, xfit, rsm_tag)

         call mod_mpi_barrier()

         ! For each individual of the population
         do i = 1, ehist%np

            ! Updating history
            ehist%hist(ehist%g,i,1:ehist%nu) = x(i,:)    ! Individual
            ehist%hist(ehist%g,i,0)    = xfit(i) ! Fitness of the individual


            ! Updating RSM Dynamic Control module
            call add_to_rsm_dynamic_control(rsm_tag(i), xfit(i), ehist%fit(i))

            ! Selecting the best individual
            if ( xfit(i) >= ehist%fit(i)) then

               ehist%pop(i,:) = x(i,:)

               ehist%fit(i) = xfit(i)

               if ( xfit(i) >= ehist%fit(ehist%ibest)) ehist%ibest = i

            end if

         end do

         ! Calculating the hybridization factor
         fh = get_hybridization_factor()



         ! If iproc == 0, master processor receives the information from slave processors,
         !                compares each individual with its trial and selects the best one,
         !                calculates the convergence coefficient and sends it to slaves.
         if (mpi%iproc == 0) then

            ! For each individual of the population
            do i = 1, ehist%np

               write(*,"(a, i4, a, 10(1pe23.15, 2x))") &
                  "The performance of the",  i, "th individual is ", xfit(i), x(i,:)

               write(21,"(2(i12), 100(2x, 1pe23.15))") ehist%g, i, xfit(i), x(i,:)

               call flush(21)

            end do

            write(20,"(i12, 3(2x, 1pe23.15),A)") ehist%g, sum(ehist%fit)/ehist%np, maxval(ehist%fit), fh

            call flush(20)

            ! Calculating the ellapsed CPU time
            call timer%measure()

            ! Saving backup data
            call ehist%save_backup(sys_var)

            call timer%save_backup()

         end if

         call mod_mpi_barrier()

      end do


      ! Master processor: data post processing
      if (mpi%iproc == 0) then

         call timer%measure()

         call write_output_files(sys_var, ehist%sname, ehist%nu, ehist%np, ehist%ibest, ehist%g, timer, &
            stopper%convergence_info(), ehist%xmin, ehist%xmax, ehist%fit, ehist%pop)

      end if


      ! Finishing MPI
      call mod_mpi_finalize()

   end associate


   end subroutine

end module
