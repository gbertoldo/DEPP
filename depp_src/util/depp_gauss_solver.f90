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

!> \brief Provides a procedure for solving linear systems by Gauss' method.

module mod_gauss_solver

   implicit none

   ! Makes everything private, except otherwise stated
   private

   public :: gausstp

contains

   !> \brief Uses Gaussian elimination to solve linear systems
   !! A . x = b with total pivoting
   !! Exit status: 0 = one solution, 1 = a zero pivot was found
   subroutine gausstp( n, a, b, x, es)
      implicit none
      integer,                 intent(in)  :: n  !< Dimension of the matrix
      real(8), dimension(n,n), intent(in)  :: a  !< Matrix
      real(8), dimension(n),   intent(in)  :: b  !< Source
      real(8), dimension(n),   intent(out) :: x  !< Solution
      integer,                 intent(out) :: es !< Exit status

      ! Inner parameters

      real(8), parameter :: eps = 1.d-14

      ! Inner variables

      integer :: i, j ! Dummy indexes

      real(8) :: raux ! Auxiliary variable

      integer, dimension(n) :: ox ! Ordering of variables

      real(8), dimension(n) :: xo ! Disordered solution

      real(8), dimension(n,n+1) :: ab ! Augmented matrix




      ! Setting augmented matrix

      ab(:,1:n) = a

      ab(:,n+1) = b


      ! Initializing ordering vector

      do i = 1, n

         ox(i) = i

      end do


      ! Initializing exit status

      es = 0


      ! Gaussian elimination

      do j = 1, n

         ! Total pivoting

         call get_total_pivoting(n, n+1-j, ab(:,j:n+1), ox(j:n) )



         if ( abs(ab(j,j)) < eps ) then

            es = 1

            return

         end if

         do i = j+1, n

            raux = - ab(i,j) / ab(j,j)

            ab(i,j:n+1) = ab(i,j:n+1) + raux * ab(j,j:n+1)

         end do

      end do


      ! Back-substitution

      do i = n, 1, -1

         xo(i) = (ab(i,n+1) - dot_product(ab(i,i+1:n),xo(i+1:n))) / ab(i,i)

      end do

      ! Reordering solution


      do i = 1, n

         x(ox(i)) = xo(i)

      end do


   end subroutine gausstp


   !> \brief Gets the maximum pivot by changing rows and columns
   subroutine get_total_pivoting( n, m, sab, ox)
      implicit none
      integer, intent(in) :: n !< Number of rows of submatrix sab
      integer, intent(in) :: m !< Number of variables os submatrix sab
      real(8), dimension(n,m+1), intent(inout) :: sab !< Augmented submatrix
      integer, dimension(m),     intent(inout) :: ox  !< Order of the variables

      ! Inner variables

      integer :: i, i1, j1

      integer :: ia, ja, iaux

      real(8) :: raux

      real(8), dimension(m+1) :: row

      real(8), dimension(n) :: col

      ! Current line

      i = n-m+1


      ! Searches for the greatest element

      ia = i

      ja = 1

      raux = abs( sab(ia,ja) )

      do i1 = i, n

         do j1 = 1, m

            if ( abs( sab( i1, j1 ) ) > raux ) then

               ia = i1

               ja = j1

               raux = abs( sab(i1,j1) )

            end if

         end do

      end do


      ! Exchanges row m with row ia

      row = sab(i,:)

      sab(i,:) = sab(ia,:)

      sab(ia,:) = row


      ! Exchanges column 1 with column ja

      col = sab(:,1)

      sab(:,1) = sab(:,ja)

      sab(:,ja) = col


      ! Updates ordering of variables

      iaux = ox(1)

      ox(1) = ox(ja)

      ox(ja) = iaux

   end subroutine get_total_pivoting

end module
