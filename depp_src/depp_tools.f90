!> \brief Contains tools for the main algorithm.
module tools

   use qsort

   implicit none

contains


   !> \brief Generates a random initial population.
   subroutine get_initial_population(g, nu, np, xmin, xmax, pop)
      implicit none
      integer, intent(out) :: g            !< generation
      integer, intent(in)  :: nu           !< number of unknowns
      integer, intent(in)  :: np           !< population size
      real(8), intent(in)  :: xmin(nu)     !< lower boundary constraints
      real(8), intent(in)  :: xmax(nu)     !< higher boundary constraints
      real(8), intent(out) :: pop(np,nu)   !< population

      integer :: j
      integer :: ind
      real(8) :: rnd

      g = 1

      do ind = 1, np

         do j = 1, nu

            call random_number(rnd)

            pop(ind,j) = xmin(j) + rnd*(xmax(j) - xmin(j))

         end do

      end do

   end subroutine get_initial_population




   !> \brief Calculates a random individual
   subroutine get_random_individual(nu, xmin, xmax, x)
      implicit none
      integer, intent(in)  :: nu          !< number of unknowns
      real(8), intent(in)  :: xmin(nu)    !< lower boundary constraints
      real(8), intent(in)  :: xmax(nu)    !< higher boundary constraints
      real(8), intent(out) :: x(nu)       !< random individual

      integer :: j   ! dummy index
      real(8) :: rnd ! random number

      do j = 1, nu

         call random_number(rnd)

         x(j) = xmin(j) + rnd*(xmax(j) - xmin(j))

      end do

   end subroutine get_random_individual




   !> \brief Selects three distinct individuals of the population.
   subroutine select_individuals(np, ind, r)
      implicit none
      integer, intent(in)  :: np    !< population size
      integer, intent(in)  :: ind   !< number of the individual
      integer, intent(out) :: r(3)  !< indexes of selected individuals

      real(8) :: rnd

      r = ind

      do while (r(1) == ind)

         call random_number(rnd)

         r(1) = int(rnd*np) + 1

      end do

      do while (r(2) == r(1) .or. r(2) == ind)

         call random_number(rnd)

         r(2) = int(rnd*np) + 1

      end do

      do while (r(3) == r(1) .or. r(3) == r(2) .or. r(3) == ind)

         call random_number(rnd)

         r(3) = int(rnd*np) + 1

      end do

   end subroutine select_individuals




   !> \brief Generates a trial individual
   subroutine get_trial_individual(ind, nu, np, kss, dif, crs, pop, fit, x)
      implicit none
      integer, intent(in)  :: ind          !< number of the individual
      integer, intent(in)  :: nu           !< number of unknowns
      integer, intent(in)  :: np           !< population size
      integer, intent(in)  :: kss          !< kind of search strategy
      real(8), intent(in)  :: dif          !< differentiation constant
      real(8), intent(in)  :: crs          !< crossover constant
      real(8), intent(in)  :: pop(np,nu)   !< population
      real(8), intent(in)  :: fit(np)      !< Fitness of the population
      real(8), intent(out) :: x(nu)        !< trial individual

      ! Selecting the search strategy
      select case (kss)

         ! Search strategy SS01: uses three distinct random individuals of the population
         ! to generate the trial individual. Strategy Rand3 of Feoktistov (2006)
         case (01)

            call get_trial_individual_ss01(ind, nu, np, dif, pop, x)

         ! Generates a trial individual from two randomly selected individuals
         ! of the population and the best individual. Search strategy SS02.
         ! Strategy Rand2-Best of Feoktistov (2006).
         case (02)

            call get_trial_individual_ss02(nu, np, dif, pop, fit, x)

         case default

            write(*,*) 'Unknown strategy. Stopping.'

            stop

      end select


      ! Crossing over the trial individual
      call crossing_over(ind, nu, np, crs, pop, x)


   end subroutine get_trial_individual





   !> \brief Generates a trial individual from three randomly selected individuals
   !! of the population. Search strategy SS01. Strategy Rand3 of Feoktistov (2006)
   subroutine get_trial_individual_ss01(ind, nu, np, dif, pop, x)
      implicit none
      integer, intent(in)  :: ind          !< number of the individual
      integer, intent(in)  :: nu           !< number of unknowns
      integer, intent(in)  :: np           !< population size
      real(8), intent(in)  :: dif          !< differentiation constant
      real(8), intent(in)  :: pop(np,nu)   !< population
      real(8), intent(out) :: x(nu)        !< trial individual

      ! Inner variables
      integer :: r(3) ! indexes of selected individuals


      ! Choosing three individuals from the population
      call select_individuals(np, ind, r)


      x = pop(r(3),:) + dif*(pop(r(1),:) - pop(r(2),:))

   end subroutine get_trial_individual_ss01




   !> \brief Generates a trial individual from two randomly selected individuals
   !! of the population and the best individual. Search strategy SS02.
   !! Strategy Rand2-Best of Feoktistov (2006)
   subroutine get_trial_individual_ss02(nu, np, dif, pop, fit, x)
      implicit none
      integer, intent(in)  :: nu           !< number of unknowns
      integer, intent(in)  :: np           !< population size
      real(8), intent(in)  :: dif          !< differentiation constant
      real(8), intent(in)  :: pop(np,nu)   !< population
      real(8), intent(in)  :: fit(np)      !< Fitness of the population
      real(8), intent(out) :: x(nu)        !< trial individual

      ! Inner variables
      integer :: ibest
      integer :: r(3) ! indexes of selected individuals

      ! Searching the index of the best individual
      ibest = maxloc(fit, dim = 1)

      ! Choosing three individuals from the population (only two of these individuals are used)
      call select_individuals(np, ibest, r)

      x = pop(ibest,:) + dif*(pop(r(1),:) - pop(r(2),:))

   end subroutine get_trial_individual_ss02





   !> \brief Performs the crossing over
   subroutine crossing_over(ind, nu, np, crs, pop, x)
      implicit none
      integer, intent(in)    :: ind          !< number of the individual
      integer, intent(in)    :: nu           !< number of unknowns
      integer, intent(in)    :: np           !< population size
      real(8), intent(in)    :: crs          !< crossover constant
      real(8), intent(in)    :: pop(np,nu)   !< population
      real(8), intent(inout) :: x(nu)        !< trial individual

      ! Inner variables
      integer :: j
      integer :: irnd
      real(8) :: rnd


      call random_number(rnd)

      irnd = int(rnd*nu) + 1

      do j = 1, nu

         call random_number(rnd)

         if (rnd < crs .or. irnd == j) then

            !x(j) = x(j)

         else

            x(j) = pop(ind,j)

         end if

      end do

   end subroutine crossing_over





   !> \brief Projects individuals into the domain of calculation
   subroutine get_X_projection(nu, xmin, xmax, x)
      implicit none
      integer, intent(in)    :: nu           !< number of unknowns
      real(8), intent(in)    :: xmin(nu)     !< lower boundary constraints
      real(8), intent(in)    :: xmax(nu)     !< higher boundary constraints
      real(8), intent(inout) :: x(nu)        !< trial individual

      integer :: j

      do j = 1, nu

         if ( x(j) < xmin(j) ) then

            x(j) = xmin(j)

         end if


         if ( x(j) > xmax(j) ) then

            x(j) = xmax(j)

         end if

      end do

   end subroutine get_X_projection





   !> \brief Get the fitness of each individual
   subroutine get_fitness(folderout, sname, fdir, ffit, ind, nu, x, xname, xfit, estatus) ! last parameter is output
      implicit none
      character(len=*), intent(in)  :: folderout   !< folder the for output files
      character(len=*), intent(in)  :: sname       !< simulations name
      character(len=*), intent(in)  :: fdir        !< name of  working directory for fitness calculation
      character(len=*), intent(in)  :: ffit        !< name of executable for fitness calculation
      integer,          intent(in)  :: ind         !< individual number
      integer,          intent(in)  :: nu          !< dimension
      real(8),          intent(in)  :: x(nu)       !< parameters for optimization
      character(10),    intent(in)  :: xname(nu)   !< names of the parameters
      real(8),          intent(out) :: xfit        !< fitness
      integer,          intent(out) :: estatus     !< exit status ( 0 = success, 1 = failure, 2 = generate another individual)

      character(3) :: char3
      character(200) :: arqpar   !< name of file for the parameters
      character(200) :: arqfit   !< name of file for the fitness
      integer :: i

      call convert_int_to_char3(ind, char3)

      arqpar = trim(folderout) // "parameters" // char3 // ".txt"
      arqfit = trim(folderout) // "fitness"    // char3 // ".txt"

      open(27, file = trim(arqpar))

      if (len(trim(arqfit)) <= 23) then
         write(27,"(a23, a)") "'" // trim(arqfit) // "'", " = arqfit: name of the file for fitness"
      else
         write(27,"(a  , a)") "'" // trim(arqfit) // "'", " = arqfit: name of the file for fitness"
      end if

      if (len(trim(sname)) <= 23) then
         write(27,"(a23, a)") trim(sname), " = sname: simulation name"
      else
         write(27,"(a  , a)") trim(sname), " = sname: simulation name"
      end if

      write(27,"(i23, a)") ind, " = ind:   individual number"
      write(27,"(i23, a)")  nu, " = nu:    number of unknowns"

      write(27,*)
      write(27,*) "=========== VARIABLES OF OPTIMIZATION ====================="
      write(27,*) "                     X                       Variable name"

      do i = 1, nu
         write(27,"(1pe23.15, a36)") x(i), trim(xname(i))
      end do

      write(27,*)
      close(27)

      call system("(cd " // trim(fdir) // " && exec ./" // trim(ffit) // ") < " // trim(arqpar) // " > /dev/null")


      open(26, file = trim(arqfit))
      read(26,*) xfit
      read(26,*) estatus
      close(26)

   end subroutine get_fitness





   !> \brief Converts an integer to a string of length 3.
   subroutine convert_int_to_char3(nmbr, char3) ! last parameter is output
      implicit none
      integer,      intent(in)  :: nmbr   !< input integer
      character(3), intent(out) :: char3  !< output string

      if (nmbr < 10) then

         write(char3,"('00',i1)") nmbr

      else if (nmbr < 100) then

         write(char3,"('0',i2)") nmbr

      else

         write(char3,"(i3)") nmbr

      end if

   end subroutine convert_int_to_char3



   !> \brief Calculates the convergence measure based on several methods
   !! kcm = 1: P-measure with dimensional variables
   !! kcm = 2: P-measure with dimensionless variables
   !! kcm = 3: P-measure based with dimensionless variables based on a fraction of the population
   subroutine get_convergence(kcm, nu, np, fc, xmin, xmax, pop, fit, cm)
      implicit none
      integer, intent(in)  :: kcm        !< Kind of convergence measure
      integer, intent(in)  :: nu         !< Dimension of the problem
      integer, intent(in)  :: np         !< Size of the population
      real(8), intent(in)  :: fc         !< Fraction of the population used for convergence measure
      real(8), intent(in)  :: xmin(nu)   !< lower boundary constraints
      real(8), intent(in)  :: xmax(nu)   !< higher boundary constraints
      real(8), intent(in)  :: pop(np,nu) !< Population
      real(8), intent(in)  :: fit(np)    !< Fitness
      real(8), intent(out) :: cm         !< Convergence measure


      select case (kcm)

         case (1) ! P-measure with dimensional variables

            call get_p_measure( 1, nu, np, xmin, xmax, pop, cm)

         case (2) ! P-measure with dimensionless variables

            call get_p_measure( 2, nu, np, xmin, xmax, pop, cm)

         case (3) ! Calculates the convergence measure of a fraction of the population

            call get_fp_measure(nu, np, fc, xmin, xmax, pop, fit, cm)

         case default

            write(*,*) "ERROR: unknown kind of convergence measure."

            stop

      end select

   end subroutine get_convergence



   !> \brief Calculates the P-measure, i. e. the population
   !! convergence measure of FEOKTISTOV "Differential evolution" 2006
   subroutine get_p_measure(kpm, nu, np, xmin, xmax, pop, pm)
      implicit none
      integer, intent(in)  :: kpm        !< Kind of P-measure (1=dimensional, 2=dimensionless)
      integer, intent(in)  :: nu         !< Dimension of the problem
      integer, intent(in)  :: np         !< Size of the population
      real(8), intent(in)  :: xmin(nu)   !< lower boundary constraints
      real(8), intent(in)  :: xmax(nu)   !< higher boundary constraints
      real(8), intent(in)  :: pop(np,nu) !< Population
      real(8), intent(out) :: pm         !< P-measure

      ! Inner variables

      integer :: j ! Dummy index

      real(8) :: raux ! Auxiliary variable

      real(8), dimension(nu)    ::   pb ! Mean individual
      real(8), dimension(np,nu) :: apop ! Auxiliary population


      if ( kpm == 1 ) then

         ! Dimensional population

         apop = pop

      else

         ! Calculation of the dimensionless population

         do j = 1, np

            apop(j,:) = ( pop(j,:) - xmin ) / ( xmax - xmin )

         end do

      end if

      ! Calculating the mean individual

      pb = 0.d0

      do j = 1, np

         pb = pb + apop(j,:)

      end do

      pb = pb / dble(np)


      ! Calculating the P-measure

      pm = 0.d0

      do j = 1, np

         raux = norma2( apop(j,:) - pb, nu )

         if ( raux > pm ) pm = raux

      end do

   end subroutine get_p_measure




   !> \brief Calculates the convergence measure based on a fraction of the population.
   !! This method takes into account the concentration of the population and its fitness.
   subroutine get_fp_measure(nu, np, fc, xmin, xmax, pop, fit, cm)
      implicit none
      integer, intent(in)  :: nu         !< Dimension of the problem
      integer, intent(in)  :: np         !< Size of the population
      real(8), intent(in)  :: fc         !< Fraction of the population
      real(8), intent(in)  :: xmin(nu)   !< lower boundary constraints
      real(8), intent(in)  :: xmax(nu)   !< higher boundary constraints
      real(8), intent(in)  :: pop(np,nu) !< Population
      real(8), intent(in)  :: fit(np)    !< Fitness
      real(8), intent(out) :: cm         !< Convergence measure

      ! Parameters

      real(8), parameter :: eps = epsilon(1.d0)


      ! Inner variables

      integer :: nc     ! Number of individuals used in the convergence measure
      integer :: j      ! Dummy index
      integer :: ox(np) ! Index of ordering

      real(8) :: pm    ! Population convergence measure
      real(8) :: fm    ! Fitness convergence measure
      real(8) :: raux1 ! Auxiliary variable
      real(8) :: raux2 ! Auxiliary variable

      real(8), dimension(np,nu) :: apop ! Auxiliary population
      real(8), dimension(np)    :: afit ! Auxiliary fitness


      ! Calculating the number of individuals used in the convergence measure
      nc = int( fc * np )


      ! Ordering the population

      do j = 1, np

         ox(j) = j

      end do

      afit = -fit

      call qsort2(afit,ox)

      do j = 1, np

         afit(j) = fit(ox(j))

         apop(j,:) = pop(ox(j),:)

      end do


      ! Calculation of the dimensionless population

      do j = 1, np

         apop(j,:) = ( apop(j,:) - xmin ) / ( xmax - xmin )

      end do


      ! Calculation of the dimensionless fitness

      if ( sqrt(eps) < abs(afit(1)) ) afit = afit / abs(afit(1))


      ! Calculating the convergence measure

      pm = 0.d0

      fm = 0.d0

      do j = 1, nc

         raux1 = norma2( apop(j,:) - apop(1,:), nu )

         raux2 = abs( afit(j) - afit(1) )

         if ( raux1 > pm ) pm = raux1

         if ( raux2 > fm ) fm = raux2

      end do

      cm = min(pm,fm)

   end subroutine get_fp_measure




   !> \brief Calculates the norm 2 of a vector
   function norma2(vec, n)
      implicit none
      integer :: n      !< dimension of the vector
      real(8) :: norma2 !< norm of the vector
      real(8) :: vec(n) !< vector

      ! Inner variables

      integer :: i
      real(8) :: soma

      soma = 0.d0

      do i = 1, n

         soma = soma + vec(i)**2

      end do

      norma2 = dsqrt(soma)

      return

   end function norma2

end module tools
