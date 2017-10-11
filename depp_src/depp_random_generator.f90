
!> \brief Provides a pseudo random number generator

module mod_random_generator
   implicit none

contains


   !> \brief Feed the random generator with a seed
   subroutine initialize_random_generator(iproc)
      implicit none
      integer, intent(in) :: iproc

      ! Inner variables
      integer :: clock     !< clock time for seeds generation
      integer :: seed(97)  !< seeds for random numbers


      ! Generating seeds for the random number subroutine
      !   call system_clock(count = clock)
      !   seed = ( iproc + 1 ) * clock
      !   call random_seed(put = seed)

   end subroutine



   !> \brief Generates a random number
   subroutine rand_number(rnd)
      implicit none
      real(8), intent(out) :: rnd

      ! Uses the GNU random number generator
      call random_number(rnd)

   end subroutine



end module
