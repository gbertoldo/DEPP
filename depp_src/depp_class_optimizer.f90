
!> \brief This module contains the optimization algorithm

module mod_class_optimizer

   use mod_global_parameters
   use mod_class_ifile
   use mod_class_system_variables
   use mod_class_ehist
   use hybrid
   use output
   use mod_mpi
   use mod_class_timer
   use mod_random_generator
   use mod_class_abstract_search_strategy
   use mod_search_strategy_factory
   use mod_class_composite_stop_condition

   implicit none

   ! Makes everything private, except otherwise stated
   private

   ! Evolution history class
   type, public :: class_optimizer

      private

      type(class_system_variables)                   :: sys_var            ! System variables
      type(class_ehist)                              :: ehist              ! Evolution history
      type(class_timer)                              :: timer              ! Timer
      class(class_abstract_search_strategy), pointer :: searcher => null() ! Search strategy object
      type(class_composite_stop_condition)           :: stopper            ! Stop condition object

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


      ! Initializing stop condition object
      call stopper%init(sys_var)


      ! if iproc == 0, master processor writes the parameter to a file
      if (mpi%iproc == 0) then

         ! Writting parameters to the output file
         call write_parameters(sys_var, ehist%sname, reload)

      end if


      ! Initializing timer
      if ( reload == 0 ) then

         call timer%start()

      else

         call timer%start(ehist%tcpu)

      end if

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
      real(8)                            :: xfit    ! Fitness of the trial individual
      real(8), dimension(:), allocatable :: x       ! Trial individual


      ! To be removed...
      real(8) :: fh =2       !< Fraction of hybridization
      integer :: rsm_tag   !< Stores the return state of application of DE-RSM


      ! Creating labels
      associate (sys_var  => this%sys_var, &
                 timer    => this%timer,   &
                 ehist    => this%ehist,   &
                 searcher => this%searcher,&
                 stopper  => this%stopper  )



      ! Allocating resources
      allocate(x(ehist%nu))


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

         ! For each individual of the population, another individual (trial individual)
         ! is created and its fitness is calculated.
         do ind = 1, ehist%np

            ! Selecting the processor
            iaux = mod(ind, mpi%nproc-1) + 1

            ! Only slave processors calculate the fitness
            if (mpi%iproc == iaux) then

               ! First generation needs special attention
               if ( ehist%g == 1 ) then

                  ! rsm_tag stores the return state of application of DE-RSM
                  rsm_tag = DE_RSM_RETURN%DE_APPLIED

                  ! Calculating the fitness function
                  fitloop1: do

                     ! Creating the trial individual x
                     call get_random_individual(ehist%nu, ehist%xmin, ehist%xmax, x)

                     call get_fitness(sys_var, ehist%sname, ind, ehist%nu, x, ehist%xname, &
                        xfit, estatus)

                     ! Analyzing the exit status of the external program
                     select case (estatus)

                        case (0) ! Success

                           exit fitloop1

                        case (1:10) ! Failure

                           ! Failure in the calculation of fitness function. Saving informations.
                           call save_fitness_failure(ehist%nu, ehist%g, ind, sys_var, ehist%sname, &
                              x, estatus)

                        case default

                           write(*,*) "ERROR: external program returned an unknown status. Stopping..."

                           stop

                     end select

                  end do fitloop1

               else


                  ! Calculating the fitness function
                  fitloop2: do

                     ! Checking if RSM can be applied

                     if ( rsm_check(ehist%np, ehist%g, fh) ) then

                        ! rsm_tag stores the return state of application of DE-RSM
                        rsm_tag = DE_RSM_RETURN%RSM_APPLIED

                        ! Generating a RSM individual

                        call get_rsm_optimum(ind, ehist%nu, ehist%np, ehist%ng, ehist%g, ehist%xmin,&
                         ehist%xmax, ehist%pop, ehist%hist, x, estatus)


                        ! If RSM fails, generates a pure DE individual
                        if ( estatus == 1 ) then

                           ! rsm_tag stores the return state of application of DE-RSM
                           rsm_tag = DE_RSM_RETURN%DE_APPLIED_AFTER_RSM_FAILURE

                           ! Creating the trial individual x
                           call searcher%get_trial(ind, ehist%pop, x)

                        end if

                     else

                        ! rsm_tag stores the return state of application of DE-RSM
                        rsm_tag = DE_RSM_RETURN%DE_APPLIED

                        ! Creating the trial individual x
                          call searcher%get_trial(ind, ehist%pop, x)

                     end if


                     ! Verifying the constraints. If the individual x is out of range,
                     ! another one is created using pure DE
                     do while ( is_X_out_of_range(ehist%nu, ehist%xmin, ehist%xmax, x) )

                        ! Checking DE-RSM status
                        select case (rsm_tag)

                           ! If DE was applied, do nothing.
                           case (DE_RSM_RETURN%DE_APPLIED)

                           ! If RSM was applied, a DE individual will be generated. Counts this application as a RSM failure.
                           case (DE_RSM_RETURN%RSM_APPLIED)

                              rsm_tag = DE_RSM_RETURN%DE_APPLIED_AFTER_RSM_FAILURE

                           ! If DE was applied after a RSM failure, counts this application as a RSM failure.
                           case (DE_RSM_RETURN%DE_APPLIED_AFTER_RSM_FAILURE)

                           ! If black box evaluation failed, do nothing.
                           case (DE_RSM_RETURN%BLACK_BOX_EVALUATION_FAILURE)

                           case default

                        end select


                        ! Creating the trial individual x
                        call searcher%get_trial(ind, ehist%pop, x)

                     end do


                     ! Asking to the external program 'ffit' the fitness of individual 'x'
                     call get_fitness(sys_var, ehist%sname, ind, ehist%nu, x, ehist%xname, &
                        xfit, estatus)

                     ! Analyzing the exit status of the external program
                     select case (estatus)

                        case (0) ! Success

                           exit fitloop2

                        case (1) ! Failure

                           ! rsm_tag stores the return state of application of DE-RSM
                           rsm_tag = DE_RSM_RETURN%BLACK_BOX_EVALUATION_FAILURE

                           ! Failure in the calculation of fitness function. Saving informations.
                           call save_fitness_failure(ehist%nu, ehist%g, ind, sys_var, ehist%sname, &
                              x, estatus)

                           x = ehist%pop(ind,:)

                           xfit = ehist%fit(ind)

                           exit fitloop2

                        case (2:10) ! Generate another individual

                           ! rsm_tag stores the return state of application of DE-RSM
                           rsm_tag = DE_RSM_RETURN%BLACK_BOX_EVALUATION_FAILURE

                           ! Failure in the calculation of fitness function. Saving informations.
                           call save_fitness_failure(ehist%nu, ehist%g, ind, sys_var, ehist%sname, &
                              x, estatus)

                        case default

                           write(*,*) "ERROR: external program returned an unknown status. Stopping..."

                           stop

                     end select

                  end do fitloop2

               end if

               ! Sending informations to master processor
               call mpi_send(    ind,  1,          mpi_integer, 0, mpi%tag, mpi%comm, mpi%code)
               call mpi_send(      x, ehist%nu, mpi_double_precision, 0, mpi%tag, mpi%comm, mpi%code)
               call mpi_send(   xfit,  1, mpi_double_precision, 0, mpi%tag, mpi%comm, mpi%code)
               call mpi_send(rsm_tag,  1,          mpi_integer, 0, mpi%tag, mpi%comm, mpi%code)

            end if

         end do

         call mod_mpi_barrier()


         ! If iproc == 0, master processor receives the information from slave processors,
         !                compares each individual with its trial and selects the best one,
         !                calculates the convergence coefficient and sends it to slaves.
         if (mpi%iproc == 0) then

            ! For each individual of the population
            do i = 1, ehist%np

               ! Selecting the processor
               iaux = mod(i, mpi%nproc-1) + 1

               ! Recieving informations from slaves
               call mpi_recv(    ind,  1,          mpi_integer, iaux, mpi%tag, mpi%comm, mpi%status, mpi%code)
               call mpi_recv(      x, ehist%nu, mpi_double_precision, iaux, mpi%tag, mpi%comm, mpi%status, mpi%code)
               call mpi_recv(   xfit,  1, mpi_double_precision, iaux, mpi%tag, mpi%comm, mpi%status, mpi%code)
               call mpi_recv(rsm_tag,  1,          mpi_integer, iaux, mpi%tag, mpi%comm, mpi%status, mpi%code)

               ! Updating history
               ehist%hist(ehist%g,ind,1:ehist%nu) = x    ! Individual
               ehist%hist(ehist%g,ind,0)    = xfit ! Fitness of the individual


               ! Updating RSM Dynamic Control module
               call add_to_rsm_dynamic_control(rsm_tag, xfit, ehist%fit(ind))


               write(*,"(a, i4, a, 10(1pe23.15, 2x))") &
                  "The performance of the",  ind, "th individual is ", xfit, x

               write(21,"(2(i12), 100(2x, 1pe23.15))") ehist%g, ind, xfit, x

               call flush(21)

               ! Selecting the best individual
               if ( xfit >= ehist%fit(ind)) then

                  ehist%pop(ind,:) = x

                  ehist%fit(ind) = xfit

                  if ( xfit >= ehist%fit(ehist%ibest)) ehist%ibest = ind

               end if

            end do


            ! Calculating the hybridization factor
            fh = get_hybridization_factor()


            ! Sending convergence measure to slave processors
            do i = 1, mpi%nproc-1
               call mpi_send(fh,    1, mpi_double_precision, i, mpi%tag, mpi%comm, mpi%code)
            end do

            write(20,"(i12, 3(2x, 1pe23.15),A)") ehist%g, sum(ehist%fit)/ehist%np, maxval(ehist%fit), fh

            call flush(20)

            ! Calculating the ellapsed CPU time
            call timer%measure()

            ! Saving backup data
            call ehist%save_backup(sys_var)

         end if

         ! If iproc /= 0, slave processors receive the convergence measure from the master processor
         if ( mpi%iproc > 0 ) then
            call mpi_recv(fh,    1, mpi_double_precision, 0, mpi%tag, mpi%comm, mpi%status, mpi%code)
         end if

         call mod_mpi_barrier()

         ! If iproc == 0, master processor sends the updated population and its fitness to
         !                slave processors
         ! If iproc /= 0, slave processors receive the updated population and its fitness
         if (mpi%iproc == 0) then

            ! Sending the population and its fitness to slave processors
            do i = 1, mpi%nproc-1
               call mpi_send(ehist%hist, ehist%ng*ehist%np*(ehist%nu+1), mpi_double_precision, i&
               , mpi%tag, mpi%comm, mpi%code)
               call mpi_send(ehist%pop,         ehist%np*ehist%nu, mpi_double_precision, i &
               , mpi%tag, mpi%comm, mpi%code)
               call mpi_send(ehist%fit,            ehist%np, mpi_double_precision, i, mpi%tag, mpi%comm, mpi%code)
            end do

         else

            ! Receiving the population and its fitness from master processor
            call mpi_recv(ehist%hist, ehist%ng*ehist%np*(ehist%nu+1), mpi_double_precision, 0&
            , mpi%tag, mpi%comm, mpi%status, mpi%code)
            call mpi_recv(ehist%pop,         ehist%np*ehist%nu, mpi_double_precision, 0&
            , mpi%tag, mpi%comm, mpi%status, mpi%code)
            call mpi_recv(ehist%fit,            ehist%np, mpi_double_precision, 0&
            , mpi%tag, mpi%comm, mpi%status, mpi%code)

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
