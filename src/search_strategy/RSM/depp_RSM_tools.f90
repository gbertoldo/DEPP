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

!> \brief Provides common procedures to all response surfaces
module mod_RSM_tools
   implicit none

contains

   !> \brief Determines if the optimum is a maximizer, minimizer or a saddle point
   subroutine get_kind_of_optimum(m, n, dm, x, f, ko)
      implicit none
      integer,                 intent(in)  :: m    !< Number of 'measures'
      integer,                 intent(in)  :: n    !< Number of variables
      real(8), dimension(m,n), intent(in)  :: dm   !< Design matrix (each row is an x point)
      real(8), dimension(n),   intent(in)  :: x    !< Critical point
      integer,                 intent(out) :: ko   !< ko: -1 = minimizer, 0 = saddle point, 1 = maximizer

      interface

         real(8) function f(x)
            implicit none
            real(8), dimension(:) :: x

         end function

      end interface


      ! Inner variables
      integer :: k
      real(8) :: fc
      real(8) :: Pc
      real(8) :: Pmin
      real(8) :: Pmax
      real(8) :: xmin(n)
      real(8) :: xmax(n)

      xmin = x
      xmax = x

      Pmax = -huge(1.d0)
      Pmin =  huge(1.d0)

      fc = f(x)


      ! Searching for maximum and minimum

      do k = 1, m

         Pc = f(dm(k,:))

         if ( Pc < Pmin ) then

            Pmin = Pc

            xmin = dm(k,:)

         end if

         if ( Pmax < Pc ) then

            Pmax = Pc

            xmax = dm(k,:)

         end if

      end do


      ! Selecting ko

      if ( fc <= Pmin) then

         ko = -1

      else if ( Pmax <= fc ) then

         ko = 1

      else

         ko = 0

      end if

   end subroutine

end module
