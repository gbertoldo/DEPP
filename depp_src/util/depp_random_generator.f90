
!> \brief Provides a pseudo random number generator

module mod_random_generator

   use mod_mpi

   implicit none

contains


   !> \brief Feed the random generator with a seed
   subroutine initialize_random_generator()
      implicit none

      ! Inner variables
      integer :: clock     !< Clock time for seeds generation
      integer :: seed(97)  !< Seeds for random numbers


      ! Generating seeds for the random number subroutine
      call system_clock(count = clock)
      seed = ( mpio%iproc + 1 ) * clock
      !call random_seed(put = seed)

   end subroutine



   !> \brief Generates a random number
   subroutine rand_number(rnd)
      implicit none
      real(8), intent(out) :: rnd !< Random number

      ! Uses the GNU Fortran Compiler random number generator
      call random_number(rnd)

   end subroutine

end module
