!> \brief Controls the stopping criteria NoAcc, i.e., if no improvement is found during a pre-defined number
!! of generations, iterations are finished.
module no_improvement_stopping_condition
   implicit none

   integer,                            private :: GNoAcc !< Maximum number of generations allowed before stopping if no improvement was found
   real(8), allocatable, dimension(:), private :: fbest  !< Register the last GNoAcc best values of the fitness function

contains


   !> \brief Initializes module
   subroutine initialize_no_improvement_stopping_condition(inGNoAcc)
      implicit none
      integer, intent(in) :: inGNoAcc !< Maximum number of generations allowed before stopping if no improvement was found

      ! Inner variables
      integer :: i

      GNoAcc = inGNoAcc

      allocate(fbest(GNoAcc))

      do i = 1, GNoAcc

         fbest(i) = -huge(1.d0)/dble(i)

      end do


   end subroutine


   !> \brief Adds the last best fitness to the list
   subroutine feed_no_improvement_stopping_condition(fit)
      implicit none
      real(8), intent(in) :: fit

      ! Inner variables
      integer, save :: ireg = 0


      ! Getting the index of the current register
      ireg = idx(ireg+1,GNoAcc)

      fbest(ireg) = fit

   end subroutine


   !> \brief Checks if the best individual of the population
   !! stagnated in the last GNoAcc generations.
   logical function is_best_fitness_stagnated()
      implicit none

      ! Inner variables
      integer :: i

      is_best_fitness_stagnated = .true.

      do i = 1, GNoAcc

         ! Checks if there are differences among the best fitness in the last GNoAcc generations
         if ( abs(fbest(i)-fbest(GNoAcc)) > 10.d0 * epsilon(1.d0) ) then

            is_best_fitness_stagnated = .false.

            return

         end if

      end do

   end function


   !> \brief Returns the index of the current register
   integer function idx(i, nps)
      implicit none
      integer, intent(in) :: i
      integer, intent(in) :: nps

      idx = mod(i,nps)

      if (idx==0) idx = nps

   end function

end module
