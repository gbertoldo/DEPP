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

!> \brief Provides an abstract class for fitting a response surface and returning its optimizer

module mod_class_abstract_RSM

   implicit none

   !> \brief Interface for a RSM object
   type, abstract, public :: class_abstract_RSM

   contains

      procedure(nfit_interface),          deferred, public, pass :: nfit            !< Minimum number of points necessary to fit the response surface
      procedure(fit_interface),           deferred, public, pass :: fit             !< Fits the response surface to the data
      procedure(P_interface),             deferred, public, pass :: P               !< Returns the value of the fitted response surface
      procedure(get_optimizer_interface), deferred, public, pass :: get_optimizer   !< Returns the response surface optimizer

   end type

   abstract interface

      !> \brief Minimum number of points necessary to fit the response surface
      integer function nfit_interface(this, n)
         import class_abstract_RSM
         implicit none
         class(class_abstract_RSM)     :: this !< A reference to this object
         integer, optional, intent(in) :: n    !< Number of unknowns

      end function

      !> \brief Fits the response surface to the data
      subroutine fit_interface(this, dm, fm, wm, es)
         import class_abstract_RSM
         implicit none
         class(class_abstract_RSM)            :: this !< A reference to this object
         real(8), dimension(:,:), intent(in)  :: dm   !< Design matrix (each row is an x point)
         real(8), dimension(:),   intent(in)  :: fm   !< 'Measures of f'
         real(8), dimension(:),   intent(in)  :: wm   !< Weight of 'Measures of f'
         integer,                 intent(out) :: es   !< Exit status: 0 = success, 1 = failure

      end subroutine

      !> \brief Returns the value of the fitted response surface
      real(8) function P_interface(this, x)
         import class_abstract_RSM
         implicit none
         class(class_abstract_RSM)         :: this !< A reference to this object
         real(8), dimension(:), intent(in) :: x    !< Independent variables

      end function

      !> \brief Returns the response surface optimizer
      subroutine get_optimizer_interface(this, dm, x, ko, es)
         import class_abstract_RSM
         implicit none
         class(class_abstract_RSM)            :: this !< A reference to this object
         real(8), dimension(:,:), intent(in)  ::   dm !< Design matrix (each row is an x point)
         real(8), dimension(:),   intent(out) ::    x !< Coordinates of the optimizer
         integer,                 intent(out) ::   ko !< ko: -1 = minimizer, 0 = saddle point, 1 = maximizer
         integer,                 intent(out) ::   es !< Exit status: 0 = success, 1 = failure

      end subroutine

   end interface

end module
