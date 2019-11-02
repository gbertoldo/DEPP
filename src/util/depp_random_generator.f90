!    DEPP - Differential Evolution Parallel Program
!
!    Copyright (C) 2019 by Jonas Joacir Radtke, Guilherme Bertoldo and Carlos Henrique Marchi
!
!    This program is free software: you can redistribute it and/or modify
!    it under the terms of the GNU General Public License as published by
!    the Free Software Foundation, either version 3 of the License, or
!    (at your option) any later version.
!
!    This program is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU General Public License for more details.
!
!    You should have received a copy of the GNU General Public License
!    along with this program.  If not, see <http://www.gnu.org/licenses/>.
!    
!    Contact:
!          Jonas Joacir Radtke (a)
!                 E-mail: jonas.radtke@gmail.com
!
!          Guilherme Bertoldo (a)
!                 E-mail: glbertoldo@gmail.com
!
!          Carlos Henrique Marchi (b)
!                 E-mail: chmcfd@gmail.com
!    Institution
!          (a) Federal University of Technology - Paraná - UTFPR
!              Linha Santa Bárbara, s/n, Francisco Beltrão, Paraná, Brazil
!              Zip Code 85601-970
!              
!          (b) Federal University of Paraná - UFPR
!              Curitiba, Paraná, Brazil
!              Caixa postal 19040
!              Zip Code 81531-980
!

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
      call random_seed(put = seed)

   end subroutine



   !> \brief Generates a random number
   subroutine rand_number(rnd)
      implicit none
      real(8), intent(out) :: rnd !< Random number

      ! Uses the GNU Fortran Compiler random number generator
      call random_number(rnd)

   end subroutine

end module
