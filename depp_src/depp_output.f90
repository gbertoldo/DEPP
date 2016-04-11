!> \brief Contains subroutines for writing output data.
module output

   implicit none

contains

   !============================================================================

   !> \brief Writes parameters in the output file
   subroutine write_parameters(folderout, sname, reload, ffit, kss, kh, &
         fh, fnb, kw, kcm, fc, nu, np, ng,  dif, crs, crsh, nstp, netol, detol, xmin, xmax)
      implicit none
      character(len=*), intent(in) :: folderout   !< folder the for output files
      character(len=*), intent(in) :: sname       !< simulations name
      character(len=*), intent(in) :: ffit        !< name of executable for fitness calculation
      integer, intent(in) :: reload    !< upload backup data
      integer, intent(in) :: kss       !< kind of search strategy
      integer, intent(in) :: kh        !< kind of the hybridization (see input file)
      real(8), intent(in) :: fh        !< Fraction of hybridization
      real(8), intent(in) :: fnb       !< Multiple of the minimum number of points for RSM fitting
      integer, intent(in) :: kw        !< kind of weighting function for RSM fitting
      integer, intent(in) :: kcm       !< kind of convergence measure
      real(8), intent(in) :: fc        !< fraction of the population used in the convergence measure
      integer, intent(in) :: nu        !< number of unknowns
      integer, intent(in) :: np        !< population size
      integer, intent(in) :: ng        !< maximal number of generations
      real(8), intent(in) :: dif       !< differentiation constant
      real(8), intent(in) :: crs       !< crossover constant
      real(8), intent(in) :: crsh      !< crossover constant of the hybridized model
      integer, intent(in) :: nstp      !< Number of trials for adjusting the step of the RSM solution
      real(8), intent(in) :: netol     !< Tolerance for distance when selecting neighbors points for RSM adjusting
      real(8), intent(in) :: detol     !< tolerance for the convergence measure in the DE algorithm
      real(8), intent(in) :: xmin(nu)  !< lower boundary constraints
      real(8), intent(in) :: xmax(nu)  !< higher boundary constraints

      character(10) :: day    ! System date
      character(8) :: hour    ! System time
      integer :: i
      logical :: lexist

      write(*,"(//, a, /)") &
         "  =======  DIFFERENTIAL EVOLUTION PARALLEL PROGRAM  =======  "

      inquire(file = trim(folderout), exist = lexist)

      if (lexist) then

         if (reload == 0) then
            call system("rm -r " // trim(folderout))
            call system("mkdir " // trim(folderout))
         end if

      else

         call system("mkdir " // trim(folderout))

      end if

      call system("cp -r ./depp_input/ " // trim(folderout))

      open(22, file = trim(folderout) // trim(sname) // "-summary.txt")

      write(22,*)
      write(22,*) " =====  DIFFERENTIAL EVOLUTION PARALLEL PROGRAM  ====="
      write(22,*)

      call date_time(day, hour)
      write(22,"(a23, a)") day, " = date:    System date"
      write(22,"(a23, a)") hour, " = time:    System time"
      write(22,*)

      if (len(trim(sname)) <= 23) then
         write(22,"(a23, a)") trim(sname), " = sname:   Simulation name"
      else
         write(22,"(a, a)") trim(sname), " = sname: Simulation name"
      end if

      if (len(trim(ffit)) <= 23) then
         write(22,"(a23, a)") trim(ffit), &
            " = ffit:    Name of executable for fitness calculation"
      else
         write(22,"(a, a)") trim(ffit), &
            " = ffit: Name of executable for fitness calculation"
      end if

      write(22,"(i23, a, a)") kss, " = kss:     Kind of search strategy"
      write(22,"(i23, a, a)") kh, " = kh:      Kind of the hybridization ", &
         "(see input file)"
      write(22,"(1pe23.15, a, a)") fh, " = fh:      Fraction of hybridization"
      write(22,"(1pe23.15, a, a)") fnb, " = fnb:     Multiple of the minimum ", &
         "number of points for RSM fitting"
      write(22,"(i23, a, a)") kw, " = kw:      Kind of weighting function ", &
         "for RSM fitting (1=uniform, 2=exponential)"
      write(22,"(i23, a)") kcm, &
         " = kcm:     Kind of convergence measure"
      write(22,"(1pe23.15, a, a)") fc, " = fc:     Fraction of the population ", &
         "used in the convergence measure (for kcm=3)"
      write(22,"(i23, a)") nu, " = nu:      Number of unknowns"
      write(22,"(i23, a)") np, " = np:      Population size"
      write(22,"(i23, a)") ng, " = ng:      Max. number of generations"
      write(22,"(1pe23.15, a)") dif, " = dif:     Differentiation constant"
      write(22,"(1pe23.15, a)") crs, " = crs:     Crossover constant"
      write(22,"(1pe23.15, a)") crsh, " = crsh:     Crossover constant of the hybridized model"
      write(22,"(i23, a)") nstp, " = nstp:      Number of trials for adjusting the step of the RSM solution"
      write(22,"(1pe23.15, a)") netol, &
         " = netol:   Tolerance for distance when selecting neighbors points for RSM adjusting"
      write(22,"(1pe23.15, a)") detol, &
         " = detol:   Tolerance for the convergence measure in the DE algorithm"

      do i = 1, nu
         write(22,"(1pe23.15, a, i2, a, i2, a)") xmin(i), &
            " = xmin(", i, "): Lower value for the ", i, " unknown"
         write(22,"(1pe23.15, a, i2, a, i2, a)") xmax(i), &
            " = xmax(", i, "): Biggest value for the ", i, " unknown"
      end do
      write(22,*)

      if (reload == 0) then

         open(20, file = trim(folderout) // trim(sname) // "-statistics.txt")
         write(20,"(A12,3(2X,A23))") "# generation", "meanfitness", "fittest", &
            "conv. meas."

         open(21, file = trim(folderout) // trim(sname) // "-history.txt")
         52 format("# generation  individual", 18x, "fitness", 15x,"parameters")
         write(21,52)

      else

         open(20, position = "append", file = trim(folderout) // trim(sname) &
            // "-statistics.txt")

         open(21, position = "append", file = trim(folderout) // trim(sname) &
            // "-history.txt")

      end if

      flush(22)

   end subroutine write_parameters

   !============================================================================

   !> \brief Calls subroutines for writing the main results, plotting statistics
   !! and convergence history.
   subroutine write_output_files(folderout, sname, nu, np, ibest, g, tcpu, &
         cm, xmin, xmax, fit, pop)
      implicit none
      character(len=*), intent(in) :: folderout  !< folder the for output files
      character(len=*), intent(in) :: sname      !< simulations name
      integer, intent(in) :: nu           !< number of unknowns
      integer, intent(in) :: np           !< population size
      integer, intent(in) :: ibest        !< index of the best individual in the population
      integer, intent(in) :: g            !< final number of generations
      real(8), intent(in) :: tcpu         !< Total CPU time
      real(8), intent(in) :: cm           !< Convergence measure
      real(8), intent(in) :: xmin(nu)     !< lower boundary constraints
      real(8), intent(in) :: xmax(nu)     !< higher boundary constraints
      real(8), intent(in) :: fit(np)      !< fitness of the population
      real(8), intent(in) :: pop(np,nu)   !< population

      close(20)
      close(21)

      call plot_statistics(sname, folderout)

      call plot_history(sname, folderout, nu, xmin, xmax)

      call write_results(tcpu, nu, np, ibest, g, cm, fit, pop)

   end subroutine

   !============================================================================

   !> \brief Writes main results to a file
   subroutine write_results(tcpu, nu, np, ibest, g, cm, fit, pop)
      implicit none
      integer, intent(in) :: nu           !< number of unknowns
      integer, intent(in) :: np           !< population size
      integer, intent(in) :: ibest        !< index of the best individual in the population
      integer, intent(in) :: g            !< final number of generations
      real(8), intent(in) :: tcpu         !< total CPU time
      real(8), intent(in) :: cm           !< Convergence measure
      real(8), intent(in) :: fit(np)      !< fitness of the population
      real(8), intent(in) :: pop(np,nu)   !< population

      character(10) tcpuf
      integer :: j

      call convert_real_to_time_format(tcpu, tcpuf)

      write(*,*)
      write(*,*) "SOLUTION:"
      do j = 1, nu
         write(*,"('X(', i2, ') = ', 1pe23.15)") j, pop(ibest,j)
      end do

      write(*,*)
      write(*,"('FITNESS: ', 1pe23.15)") fit(ibest)
      write(*,*)

      write(22,*)
      write(22,*) " ====================  SOLUTION  ===================== "
      write(22,*)

      do j = 1, nu
         write(22,"(1pe23.15, a, i2, a, i2, a)") pop(ibest,j), &
            " = x(", j, "):    The best value for the ", j, " unknown"
      end do

      write(22,*)
      write(22,"(1pe23.15, a)") fit(ibest), " = fittest: The best fitness found"
      write(22,*)

      write(22,"(I23, a)") g, " = g:       Final number of generations"
      write(22,"(1pe23.15, a)") cm, " = cm:      Convergence measure"
      write(22,"(1pe23.15, a)") tcpu, " = tcpu:    Total CPU time [s]"
      write(22,"(a23, a)") tcpuf, &
         " = tcpuf:   Total CPU time formatted [hh:mm:ss]"
      write(22,*)

      close(22)

   end subroutine write_results

   !============================================================================

   !> \brief Saves data about the failure in the calculation of the fitness
   subroutine save_fitness_failure(nu, g, ind, folderout, sname, x, estatus)
      implicit none
      integer, intent(in) ::  nu !< number of unknowns
      integer, intent(in) ::   g !< number of the generation
      integer, intent(in) :: ind !< number of the individual
      integer, intent(in) :: estatus   !< exit status (0=success; 1=failure; 2=generate another individual)

      character(len=*), intent(in) :: folderout !< folder the for output files
      character(len=*), intent(in) :: sname     !< simulations name

      real(8), intent(in) :: x(nu) !< unknowns


      ! Inner variables

      integer :: i ! Dummy index
      character(3) :: str1 ! Auxiliary string 1
      character(3) :: str2 ! Auxiliary string 2
      character(250) :: fout ! Output file name


      ! Creating the name of the output file

      call convert_int_to_char3(g,str1)

      call convert_int_to_char3(ind,str2)

      fout = trim(adjustl(folderout)) // "fitness_failures_i" // str2 // ".txt"

      open(10, file = fout, position = "append" )

      if (len(trim(adjustl(sname))) > 23) then
         write(10,*) trim(adjustl(sname)), " = sname:    simulation name"
      else
         write(10,"(A23,A)") trim(adjustl(sname)), " = sname:    simulation name"
      end if

      write(10,"(I23,A)") g, " = g:        generation number"
      write(10,"(I23,A)") ind, " = ind:      individual number"
      write(10,"(I23,A)") estatus, " = estatus:  exit status"

      do i = 1, nu

         write(10,"(ES23.16,A,I2,A)") x(i), " = x(", i, "):"

      end do

      write(10,*)

      close(10)

   contains

      subroutine convert_int_to_char3(nmbr, char3) ! last parameter is output
         implicit none
         character(3), intent(out) :: char3
         integer, intent(in) :: nmbr

         if (nmbr < 10) then
            write(char3,"('00',i1)") nmbr
         else if (nmbr < 100) then
            write(char3,"('0',i2)") nmbr
         else
            write(char3,"(i3)") nmbr
         end if

      end subroutine convert_int_to_char3

   end subroutine save_fitness_failure


   !============================================================================

   ! \brief Saves a backup of some important data
   subroutine save_backup(folderout, sname, ng, nu, np, tcpu, g, fit, pop, hist)
      implicit none
      character(len=*), intent(in) :: folderout   !< folder the for output files
      character(len=*), intent(in) :: sname       !< simulations name
      integer, intent(in) :: ng               !< maximum number of generations
      integer, intent(in) :: nu               !< dimension of the problem
      integer, intent(in) :: np               !< size of the population
      integer, intent(in) :: g                !< generation
      real(8), intent(in) :: tcpu             !< accumulated CPU time
      real(8), intent(in) :: fit(np)          !< fitness of the population
      real(8), intent(in) :: pop(np,nu)       !< population of chromosomes
      real(8), intent(in) :: hist(ng,np,0:nu) !< history


      ! Inner variables

      integer :: ind, cg ! Dummy index

      open(23, file = trim(folderout) // trim(sname) // "-backup.txt")

      write(23,"(1pe23.15, a)") tcpu, " = tcpu:    Accumulated CPU time"

      write(23,    "(i23, a)")    g, " = g:       Last generation"

      write(23,*)

      do ind = 1, np

         write(23,"(100(1pe23.15, 2x))") fit(ind), pop(ind,:)

      end do

      write(23,*)

      do cg = 1, g

         do ind = 1, np

            write(23,"(101(1pe23.15, 2x))") hist(cg, ind, :)

         end do

      end do

      write(23,*)

      close(23)

   end subroutine save_backup

   !============================================================================

   !> \brief Converts a real number to the time format 00:00:00.
   subroutine convert_real_to_time_format(rtime, ftime)
      implicit none
      character(10), intent(out) :: ftime !< time formatted
      real(8), intent(in) :: rtime        !< time in seconds

      character(2) :: char2
      integer :: hora
      integer :: min
      integer :: seg

      hora = int(rtime/3600)
      min = int((rtime - hora*3600)/60)
      seg = int(rtime - hora*3600 - min*60)

      write(ftime, "(i4)") hora

      write(char2, "(i2)") min
      if (min < 10) char2 = "0" // adjustl(char2)
      ftime = trim(ftime) // ":" // char2

      write(char2, "(i2)") seg
      if (seg < 10) char2 = "0" // adjustl(char2)
      ftime = trim(ftime) // ":" // char2

   end subroutine convert_real_to_time_format

   !============================================================================

   !> \brief Plots the fittest, the mean and the convergence measure of the
   !! population.
   subroutine plot_statistics(sname, folderout)
      implicit none
      character(len=*), intent(in) :: sname       !< simulations name
      character(len=*), intent(in) :: folderout   !< folder the for output files
      character(len=600) :: fname

      open(48, file = trim(folderout) // trim(sname) // "-statistics.plt")

      write(48,*) "set terminal postscript eps rounded color"
      write(48,*) "set out '", trim(folderout), trim(sname), "-statistics.eps'"
      write(48,*) "set style data linespoints"
      write(48,*) "set time"
      write(48,*) "set title 'statistics of ", trim(sname), "'"
      write(48,*) "set xlabel 'generation  (g)'"
      write(48,*) "set ylabel 'fitness'"
      write(48,*) "set grid"
      fname = trim(folderout) // trim(sname) // "-statistics.txt"
      write(48,*) "plot '", trim(fname), "' using 1:3 pt 4 title 'Fittest',\"
      fname = trim(folderout) // trim(sname) // "-statistics.txt"
      write(48,*) "     '", trim(fname), "' using 1:2 pt 6 title 'Mean'"
      write(48,*) "exit"

      close(48)

      call system("gnuplot -persist "// trim(folderout) // trim(sname) // &
         "-statistics.plt")

      open(49, file = trim(folderout) // trim(sname) // "-convergence.plt")

      write(49,*) "set terminal postscript eps rounded color"
      write(49,*) "set out '", trim(folderout), trim(sname), "-convergence.eps'"
      write(49,*) "set style data linespoints"
      write(49,*) "set time"
      write(49,*) "set logscale y"
      write(49,*) "set title 'convergence measure of ", trim(sname), "'"
      write(49,*) "set xlabel 'generation  (g)'"
      write(49,*) "set ylabel 'convergence measure'"
      write(49,*) "set grid"
      fname = trim(folderout) // trim(sname) // "-statistics.txt"
      write(49,*) "plot '", trim(fname) , "' u 1:4 pt 5 t 'Convergence measure'"
      write(49,*) "exit"

      close(49)

      call system("gnuplot -persist "// trim(folderout) // trim(sname) // &
         "-convergence.plt")

   end subroutine plot_statistics

   !============================================================================

   !> \brief Plots all the individuals generated and their fitness for 1D and 2D
   !! problems.
   subroutine plot_history(sname, folderout, nu, xmin, xmax)
      implicit none
      character(len=*), intent(in) :: sname       !< simulations name
      character(len=*), intent(in) :: folderout   !< folder the for output files
      integer, intent(in) :: nu        !< number of unknowns
      real(8), intent(in) :: xmin(nu)  !< lower boundary constraints
      real(8), intent(in) :: xmax(nu)  !< higher boundary constraints

      character(200) :: fname
      integer :: i
      integer :: j
      integer :: k
      integer :: m
      integer :: io
      integer :: ns
      real(8) :: hx
      real(8) :: hy
      real(8) :: xx
      real(8) :: yy
      real(8) :: raux1
      real(8) :: raux2
      real(8) :: raux3
      real(8), allocatable :: par1(:)
      real(8), allocatable :: par2(:)
      real(8), allocatable :: fitness(:)

      if (nu == 2) then

         open(46, file = trim(folderout) // trim(sname) // "-history.txt")

         ns = 0
         read(46,*)
         do
            read(46,*,iostat=io) j, k, raux1, raux2, raux3

            if ( io /= 0 ) exit

            ns = ns + 1

         end do
         rewind(46)

         allocate( par1(ns) )
         allocate( par2(ns) )
         allocate( fitness(ns) )

         read(46,*)
         do i = 1, ns
            read(46,*) j, k, fitness(i), par1(i), par2(i)
         end do

         close(46)

         open(45, file = trim(folderout) // trim(sname) // "-field.txt")

         write(45,*) "#   parameter 1      parameter 2          fitness"

         m = 256
         hx = (xmax(1) - xmin(1))/(m - 1)
         hy = (xmax(2) - xmin(2))/(m - 1)

         do j = 1, m

            do i = 1, m

               xx = xmin(1) + (i - 1)*hx
               yy = xmin(2) + (j - 1)*hy

               call get_nearest(xx, yy, hx, hy, ns, par1, par2, k)

               write(45,"(3(1pe15.8, 2x))") xx, yy, fitness(k)

            end do

            write(45,*)

         end do

         write(45,*)

         close(45)

      end if

      open(47, file = trim(folderout) // trim(sname) // "-history.plt")

      write(47,*) "set terminal postscript eps rounded color"
      write(47,*) "set out '", trim(folderout), trim(sname), "-history.eps'"
      write(47,*) "set time"
      write(47,*) "set title 'history of ", trim(sname), "'"

      if (nu == 1) then

         write(47,*) "set style data points"
         write(47,*) "set grid"
         write(47,*) "set xrange [", xmin(1), ":", xmax(1), "]"
         write(47,*) "set xlabel 'parameter'"
         write(47,*) "set ylabel 'fitness'"
         fname = trim(folderout) // trim(sname) // "-history.txt"
         write(47,*) "plot '", trim(fname), "' using 4:3 pt 4 title ''"

      else if (nu == 2) then

         write(47,*) "set style data pm3d"
         write(47,*) "set ticslevel 0"
         write(47,*) "set view map"
         write(47,*) "set pm3d map"
         write(47,*) "set palette"
         write(47,*) "set xrange [", xmin(1), ":", xmax(1), "]"
         write(47,*) "set yrange [", xmin(2), ":", xmax(2), "]"
         write(47,*) "set xlabel 'parameter 1'"
         write(47,*) "set ylabel 'parameter 2'"
         write(47,*) "set zlabel 'fitness'"
         fname = trim(folderout) // trim(sname) // "-field.txt"
         write(47,*) "splot '", trim(fname), "' using 1:2:3 title ''"

      end if

      write(47,*) "exit"

      close(47)

      if (nu == 1 .or. nu == 2) then
         call system("gnuplot -persist "// trim(folderout) // trim(sname) // &
            "-history.plt")
      end if

   end subroutine plot_history

   !============================================================================

   !> \brief Finds the nearest individual to a given point.
   subroutine get_nearest(xx, yy, hx, hy, m, par1, par2, k)
      implicit none
      integer, intent(in) :: m
      integer, intent(out) :: k
      real(8), intent(in) :: xx
      real(8), intent(in) :: yy
      real(8), intent(in) :: hx
      real(8), intent(in) :: hy
      real(8), intent(in) :: par1(m)
      real(8), intent(in) :: par2(m)

      integer :: i
      real(8) :: ld
      real(8) :: dist

      k = 1
      ld = sqrt(((xx - par1(1))/hx)**2 + ((yy - par2(1))/hy)**2)

      do i = 1, m

         dist = sqrt(((xx - par1(i))/hx)**2 + ((yy - par2(i))/hy)**2)

         if (dist < ld) then
            k = i
            ld = dist
         end if

      end do

   end subroutine get_nearest

   !============================================================================

   !> \brief Obtains system day and hour
   subroutine date_time(day, hour)
      implicit none
      character(10), intent(out) :: day   !< system date
      character(8), intent(out) :: hour   !< system time

      character(2)  :: aux1
      character(2)  :: aux2
      character(4)  :: aux3
      character(20) :: vardate   ! system date
      character(20) :: vartime   ! system time
      character(20) :: varzone
      character(50) :: aux
      integer       :: var(8)    ! date and time

      call date_and_time(vardate, vartime, varzone, var)

      write(aux,*) var(3)
      aux1 = trim(adjustl(aux))
      if (var(3) < 10) aux1 = "0" // trim(adjustl(aux1))
      write(aux,*) var(2)
      aux2 = trim(adjustl(aux))
      if (var(2) < 10) aux2 = "0" // trim(adjustl(aux2))
      write(aux,*) var(1)
      aux3 = trim(adjustl(aux))
      day = trim(aux1) // '/' // trim(aux2) // '/' // aux3

      write(aux,*) var(5)
      aux1 = trim(adjustl(aux))
      if (var(5) < 10) aux1 = "0" // trim(adjustl(aux1))
      write(aux,*) var(6)
      aux2 = trim(adjustl(aux))
      if (var(6) < 10) aux2 = "0" // trim(adjustl(aux2))
      write(aux,*) var(7)
      aux3 = trim(adjustl(aux))
      if (var(7) < 10) aux3 = "0" // trim(adjustl(aux3))
      hour = trim(aux1) // ':' // trim(aux2) // ':' // aux3

   end subroutine date_time

   !============================================================================

end module output
