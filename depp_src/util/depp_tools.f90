!> \brief Contains tools for the main algorithm.
module tools

   use qsort

   use mod_random_generator

   use mod_class_system_variables

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

            call rand_number(rnd)

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

         call rand_number(rnd)

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

         call rand_number(rnd)

         r(1) = int(rnd*np) + 1

      end do

      do while (r(2) == r(1) .or. r(2) == ind)

         call rand_number(rnd)

         r(2) = int(rnd*np) + 1

      end do

      do while (r(3) == r(1) .or. r(3) == r(2) .or. r(3) == ind)

         call rand_number(rnd)

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


      call rand_number(rnd)

      irnd = int(rnd*nu) + 1

      do j = 1, nu

         call rand_number(rnd)

         if (rnd < crs .or. irnd == j) then

            !x(j) = x(j)

         else

            x(j) = pop(ind,j)

         end if

      end do

   end subroutine crossing_over





   !> \brief Checks if X is out of range
   logical function is_X_out_of_range(nu, xmin, xmax, x)
      implicit none
      integer, intent(in)    :: nu           !< number of unknowns
      real(8), intent(in)    :: xmin(nu)     !< lower boundary constraints
      real(8), intent(in)    :: xmax(nu)     !< higher boundary constraints
      real(8), intent(in)    :: x(nu)        !< trial individual

      integer :: j

      is_X_out_of_range = .false.

      do j = 1, nu

         if ( x(j) < xmin(j) ) then

            is_X_out_of_range = .true.

            return

         end if


         if ( x(j) > xmax(j) ) then

            is_X_out_of_range = .true.

            return

         end if

      end do

   end function is_X_out_of_range





   !> \brief Get the fitness of each individual
   subroutine get_fitness(sys_var, sname, ind, nu, x, xname, xfit, estatus) ! last parameter is output
      implicit none
      class(class_system_variables), intent(in) :: sys_var
      character(len=*), intent(in)  :: sname       !< simulations name
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

      arqpar = trim(sys_var%absfolderout) // "parameters" // char3 // ".txt"
      arqfit = trim(sys_var%absfolderout) // "fitness"    // char3 // ".txt"

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

      call system("(cd " // trim(sys_var%fdir) // " && exec ./" // trim(sys_var%ffit) // ") < " // trim(arqpar) // " > /dev/null")


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

end module tools
