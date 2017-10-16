!> \mainpage DEPP
!!
!! \par DIFFERENTIAL EVOLUTION PARALLEL PROGRAM 2.0
!!
!! \authors
!!
!!          Jonas Joacir Radtke
!!
!!                 E-mail: jonas.radtke@gmail.com
!!             Curriculum: http://lattes.cnpq.br/7202527344656915
!!                    URL: http://paginapessoal.utfpr.edu.br/jonas
!!
!!
!!          Guilherme Bertoldo
!!
!!                 E-mail: glbertoldo@gmail.com
!!             Curriculum: http://buscatextual.cnpq.br/buscatextual/visualizacv.do?id=H769069
!!
!!
!! \par Institution
!!          Federal University of Technology - Paraná - UTFPR
!!
!!
!! \date Apr, 2016.
!!
!! \version 2.0
!!
!! \par SVN CHECKOUT
!!          http://depp.googlecode.com/svn/trunk/
!!
!! \section Introduction
!!
!! This is the documentation of the program DEPP, which implements the Differential
!! Evolution (DE) algorithm for non-linear optimization within a rectangular domain.
!! This algorithm also contains an hybridization with the Response Surface Methodology
!! (RSM) that may accelerate the convergence. The program deals only with maximization.
!! Minimization can be taken into account by multiplying the fitness function by -1.
!! The code implementation is MPI-FORTRAN 90.

!> \brief Main program
program depp

   use input
   use hybrid
   use output
   use stopping_condition_module
   use mod_mpi
   use mod_class_timer
   use mod_random_generator

   implicit none

   integer       :: i                        !< Dummy variable
   integer       :: iaux                     !< Dummy variable
   real(8)       :: tcpu0
   type(class_timer) :: timer


   ! Initializing MPI module
   call initialize_mpi_module()


   ! Initializing random number generator module
   !call initialize_random_generator(iproc)





   ! Getting the input data
   call get_parameters()
!     folderin,  folderout,  sname,  iarq, reload, fdir,  ffit,   &
!      kss, kh, fh, fhmin, fhmax, fhm, fnb, kw, kpm, nu, np, ng, GNoAcc, dif,         &
!      crs, crsh, nstp, netol, detol, xmin, xmax, xname, x, fit, pop, hist)


   ! Initializes hybrid module and checks hybridization necessary condition for RSM
   call initialize_hybrid_module(sys_var,es)


   ! Initializers stopping condition module
   call initialize_stopping_condition_module(sys_var)


   ! Checking the exit status of the module initialization
   if ( es == 1 ) then

      ! Finishing MPI
      call mpi_abort(comm, code, code)

   end if


   ! if iproc == 0, master processor writes the parameter to a file
   if (iproc == 0) then

      ! Writting parameters to the output file
      call write_parameters(sys_var, sname, reload)

   end if

   ! If reload=0, data is initialized, otherwise the population and its fitness are read from the backup file
   if ( reload == 0 ) then

      call timer%start()

      g    =  0
      pop  =  0.d0
      fit  = -huge(1.d0)
      hist = 0.d0
      ibest = 1

   else

      ! Loading data
      call load_backup(sys_var, sname, ng, nu, np, tcpu0, g, fit, pop, hist)

      call timer%start(tcpu0)

      ! Searching for the best individual of the current generation
      ibest = maxloc(fit,1)

   end if

   call mpi_barrier(comm, code)


   ! Starting the generations loop. This loop is maintained while the stopping
   ! condition is not satisfied
   do

      call compute_stop_condition(nu, np, g, xmin, xmax, pop, fit)

      ! Printing convergence measure of the current generation
      if (iproc==0) then

         write(*,*)  g, trim(convergence_info)
         write(24,*) g, trim(convergence_info)
         call flush(24)

      end if

      if ( is_stop_condition_satisfied() ) exit


      ! Starting a new generation

      g = g+1

      ! Print time
      if (iproc == 0) then

         call timer%measure()

         write(*,"(/, a, a)") "Accumulated CPU time: ", timer%formatted_elapsed_time()

         write(*,"(/, a, i4, a, /)") "Processing the", g, "th generation..."

      end if

      ! For each individual of the population, another individual (trial individual)
      ! is created and its fitness is calculated.
      do ind = 1, np

         ! Selecting the processor
         iaux = mod(ind, nproc-1) + 1

         ! Only slave processors calculate the fitness
         if (iproc == iaux) then

            ! First generation needs special attention
            if ( g == 1 ) then

               ! rsm_tag stores the return state of application of DE-RSM
               rsm_tag = DE_RSM_RETURN%DE_APPLIED

               ! Calculating the fitness function
               fitloop1: do

                  ! Creating the trial individual x
                  call get_random_individual(nu, xmin, xmax, x)

                  call get_fitness(sys_var, sname, ind, nu, x, xname, &
                     xfit, estatus)

                  ! Analyzing the exit status of the external program
                  select case (estatus)

                     case (0) ! Success

                        exit fitloop1

                     case (1:10) ! Failure

                        ! Failure in the calculation of fitness function. Saving informations.
                        call save_fitness_failure(nu, g, ind, sys_var, sname, &
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

                  if ( rsm_check(np, g, fh) ) then

                     ! rsm_tag stores the return state of application of DE-RSM
                     rsm_tag = DE_RSM_RETURN%RSM_APPLIED

                     ! Generating a RSM individual

                     call get_rsm_optimum(ind, nu, np, ng, g, xmin, xmax, pop, hist, x, es)


                     ! If RSM fails, generates a pure DE individual
                     if ( es == 1 ) then

                        ! rsm_tag stores the return state of application of DE-RSM
                        rsm_tag = DE_RSM_RETURN%DE_APPLIED_AFTER_RSM_FAILURE

                        ! Creating the trial individual x
                        call get_trial_individual(ind, nu, np, kss, dif, crs, pop, fit, x)

                     end if

                  else

                     ! rsm_tag stores the return state of application of DE-RSM
                     rsm_tag = DE_RSM_RETURN%DE_APPLIED

                     ! Creating the trial individual x
                     call get_trial_individual(ind, nu, np, kss, dif, crs, pop, fit, x)

                  end if


                  ! Verifying the constraints. If the individual x is out of range,
                  ! another one is created using pure DE
                  do while ( is_X_out_of_range(nu, xmin, xmax, x) )

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
                     call get_trial_individual(ind, nu, np, kss, dif, crs, pop, fit, x)

                  end do


                  ! Asking to the external program 'ffit' the fitness of individual 'x'
                  call get_fitness(sys_var, sname, ind, nu, x, xname, &
                     xfit, estatus)

                  ! Analyzing the exit status of the external program
                  select case (estatus)

                     case (0) ! Success

                        exit fitloop2

                     case (1) ! Failure

                        ! rsm_tag stores the return state of application of DE-RSM
                        rsm_tag = DE_RSM_RETURN%BLACK_BOX_EVALUATION_FAILURE

                        ! Failure in the calculation of fitness function. Saving informations.
                        call save_fitness_failure(nu, g, ind, sys_var, sname, &
                           x, estatus)

                        x = pop(ind,:)

                        xfit = fit(ind)

                        exit fitloop2

                     case (2:10) ! Generate another individual

                        ! rsm_tag stores the return state of application of DE-RSM
                        rsm_tag = DE_RSM_RETURN%BLACK_BOX_EVALUATION_FAILURE

                        ! Failure in the calculation of fitness function. Saving informations.
                        call save_fitness_failure(nu, g, ind, sys_var, sname, &
                           x, estatus)

                     case default

                        write(*,*) "ERROR: external program returned an unknown status. Stopping..."

                        stop

                  end select

               end do fitloop2

            end if

            ! Sending informations to master processor
            call mpi_send(    ind,  1,          mpi_integer, 0, tag, comm, code)
            call mpi_send(      x, nu, mpi_double_precision, 0, tag, comm, code)
            call mpi_send(   xfit,  1, mpi_double_precision, 0, tag, comm, code)
            call mpi_send(rsm_tag,  1,          mpi_integer, 0, tag, comm, code)

         end if

      end do

      call mpi_barrier(comm, code)


      ! If iproc == 0, master processor receives the information from slave processors,
      !                compares each individual with its trial and selects the best one,
      !                calculates the convergence coefficient and sends it to slaves.
      if (iproc == 0) then

         ! For each individual of the population
         do i = 1, np

            ! Selecting the processor
            iaux = mod(i, nproc-1) + 1

            ! Recieving informations from slaves
            call mpi_recv(    ind,  1,          mpi_integer, iaux, tag, comm, status, code)
            call mpi_recv(      x, nu, mpi_double_precision, iaux, tag, comm, status, code)
            call mpi_recv(   xfit,  1, mpi_double_precision, iaux, tag, comm, status, code)
            call mpi_recv(rsm_tag,  1,          mpi_integer, iaux, tag, comm, status, code)

            ! Updating history
            hist(g,ind,1:nu) = x    ! Individual
            hist(g,ind,0)    = xfit ! Fitness of the individual


            ! Updating RSM Dynamic Control module
            call add_to_rsm_dynamic_control(rsm_tag, xfit, fit(ind))


            write(*,"(a, i4, a, 10(1pe23.15, 2x))") &
               "The performance of the",  ind, "th individual is ", xfit, x

            write(21,"(2(i12), 100(2x, 1pe23.15))") g, ind, xfit, x

            call flush(21)

            ! Selecting the best individual
            if ( xfit >= fit(ind)) then

               pop(ind,:) = x

               fit(ind) = xfit

               if ( xfit >= fit(ibest)) ibest = ind

            end if

         end do


         ! Calculating the hybridization factor
         fh = get_hybridization_factor()


         ! Sending convergence measure to slave processors
         do i = 1, nproc-1
            call mpi_send(fh,    1, mpi_double_precision, i, tag, comm, code)
         end do

         write(20,"(i12, 3(2x, 1pe23.15),A)") g, sum(fit)/np, maxval(fit), fh

         call flush(20)

         ! Calculating the ellapsed CPU time
         call timer%measure()

         ! Saving backup data
         call save_backup(sys_var, sname, ng, nu, np, timer%elapsed_time(), g, fit, pop, hist)

      end if

      ! If iproc /= 0, slave processors receive the convergence measure from the master processor
      if ( iproc > 0 ) then
         call mpi_recv(fh,    1, mpi_double_precision, 0, tag, comm, status, code)
      end if

      call mpi_barrier(comm, code)

      ! If iproc == 0, master processor sends the updated population and its fitness to
      !                slave processors
      ! If iproc /= 0, slave processors receive the updated population and its fitness
      if (iproc == 0) then

         ! Sending the population and its fitness to slave processors
         do i = 1, nproc-1
            call mpi_send(hist, ng*np*(nu+1), mpi_double_precision, i, tag, comm, code)
            call mpi_send(pop,         np*nu, mpi_double_precision, i, tag, comm, code)
            call mpi_send(fit,            np, mpi_double_precision, i, tag, comm, code)
         end do

      else

         ! Receiving the population and its fitness from master processor
         call mpi_recv(hist, ng*np*(nu+1), mpi_double_precision, 0, tag, comm, status, code)
         call mpi_recv(pop,         np*nu, mpi_double_precision, 0, tag, comm, status, code)
         call mpi_recv(fit,            np, mpi_double_precision, 0, tag, comm, status, code)

      end if

      call mpi_barrier(comm, code)

   end do


   ! Master processor: data post processing
   if (iproc == 0) then

      call timer%measure()

      call write_output_files(sys_var, sname, nu, np, ibest, g, timer, &
         convergence_info, xmin, xmax, fit, pop)

   end if


   ! Finishing MPI
   call mpi_finalize(code)

end program depp
