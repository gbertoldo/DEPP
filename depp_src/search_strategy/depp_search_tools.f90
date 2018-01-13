!> \brief Contains tools for DE search.
module mod_search_tools

   use mod_random_generator

   implicit none

contains


   !> \brief Calculates a random individual
   subroutine get_random_individual(nu, xmin, xmax, x)
      implicit none
      integer, intent(in)  :: nu          !< Number of unknowns
      real(8), intent(in)  :: xmin(nu)    !< Lower boundary constraints
      real(8), intent(in)  :: xmax(nu)    !< Upper boundary constraints
      real(8), intent(out) :: x(nu)       !< Random individual

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
      integer, intent(in)  :: np    !< Population size
      integer, intent(in)  :: ind   !< Number of the individual
      integer, intent(out) :: r(3)  !< Indexes of selected individuals

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


   !> \brief Performs the crossing over
   subroutine crossing_over(ind, nu, np, crs, pop, x)
      implicit none
      integer, intent(in)    :: ind          !< Number of the individual
      integer, intent(in)    :: nu           !< Number of unknowns
      integer, intent(in)    :: np           !< Population size
      real(8), intent(in)    :: crs          !< Crossover constant
      real(8), intent(in)    :: pop(np,nu)   !< Population
      real(8), intent(inout) :: x(nu)        !< Trial individual

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
      integer, intent(in)    :: nu           !< Number of unknowns
      real(8), intent(in)    :: xmin(nu)     !< Lower boundary constraints
      real(8), intent(in)    :: xmax(nu)     !< Upper boundary constraints
      real(8), intent(in)    :: x(nu)        !< Trial individual

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

end module
