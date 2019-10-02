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

!> \brief Implements the Quicksort Algorithm
module qsort
   implicit none

contains

   !> \brief Sorts a vector x based on the Quicksort Algorithm.
   !! It also returns the ordering ox of the original vector.
   recursive subroutine qsort2(x,ox)
      implicit none
      real(8), dimension(:), intent(inout) ::  x !< Unordered vector
      integer, dimension(:), intent(inout) :: ox !< Ordering of x

      ! Inner variables

      integer :: ip ! Split position

      if ( size(x) > 1 ) then

         call partition2( x, ip, ox )

         call qsort2( x(:ip-1), ox(:ip-1) )

         call qsort2( x(ip:), ox(ip:) )

      end if

   end subroutine qsort2


   !> \brief Separates the number lower and greater than the pivot
   subroutine partition2(x, ip, ox)
      implicit none
      real(8), dimension(:), intent(inout) :: x  !< Unordered vector
      integer,                 intent(out) :: ip !< Split position
      integer, dimension(:), intent(inout) :: ox !< Ordering of x

      ! Inner variables

      integer :: left, right, iaux, imin, imax

      real(8) :: pivot, raux

      ! Initializing sweepers

      left = 0

      right = size(x) + 1


      ! Pivot is the median of the initial, mean and final index

      iaux =  int( ( 1 + size(x) ) / 2 )

      imin = minloc([x(1), x(iaux), x(size(x))],1)

      imax = maxloc([x(1), x(iaux), x(size(x))],1)

      if ( ( imin == 2 .and. imax == 3 ) .or. ( imin == 3 .and. imax == 2 ) ) then

         pivot = x(1)

      else if ( ( imin == 1 .and. imax == 3 ) .or. ( imin == 3 .and. imax == 1 ) ) then

         pivot = x(iaux)

      else

         pivot = x(size(x))

      end if



      ! Sweeping unordered vector: right -> left and left -> right

      do while ( left < right )


         ! Sweeping unordered vector: right -> left

         right = right - 1

         do while ( pivot < x(right) )

            right = right - 1

         end do


         ! Sweeping unordered vector: left -> right

         left = left + 1

         do while ( x(left) < pivot )

            left = left + 1

         end do


         if ( left < right ) then

            raux = x(left)

            x(left) = x(right)

            x(right) = raux

            iaux = ox(left)

            ox(left) = ox(right)

            ox(right) = iaux

         end if

      end do

      if ( left == right ) then

         ip = left + 1

      else

         ip = left

      end if

   end subroutine partition2

end module qsort
