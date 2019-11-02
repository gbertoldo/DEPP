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

!> \brief Provides a class for fitting a quadratic response surface and returning its optimizer

module mod_class_RSM_Quadratic_Model

   use mod_class_abstract_RSM
   use mod_gauss_solver
   use mod_RSM_tools
   use mod_mpi

   implicit none

   !> \brief Quadratic response surface model
   type, public, extends(class_abstract_RSM) :: class_RSM_Quadratic_Model

      private

      integer                             :: nb !< Number of base functions
      integer                             :: m  !< Number of 'measures'
      integer                             :: n  !< Number of unknowns
      real(8), allocatable, dimension(:)  :: cp !< Coefficients

   contains

      procedure, public,  pass :: nfit          !< Minimum number of points necessary to fit the response surface
      procedure, public,  pass :: fit           !< Fits the polynomial to the data
      procedure, public,  pass :: P             !< Returns the value of the fitted response surface for a given x
      procedure, public,  pass :: get_optimizer !< Returns the response surface optimizer
      procedure, private, pass :: phi           !< Base function

   end type

contains


   !> \brief Minimum number of points necessary to fit the response surface
   integer function nfit(this, n)
      implicit none
      class(class_RSM_Quadratic_Model) :: this !< A reference to this object
      integer, optional, intent(in)    :: n    !< Number of unknowns

      if ( present(n) ) then

         nfit = (n+1)*(n+2)/2

      else

         nfit = this%nb

      end if

   end function


   !> \brief Fits the response surface to the data.
   !! Calculates the coefficients c_p that fits the model function
   !!
   !! y(x) = sum( c_p * phi_p(x) , p = 1 .. nb )
   !!
   !! to the function f(x).
   !!
   subroutine fit(this, dm, fm, wm, es)
      implicit none
      class(class_RSM_Quadratic_Model)     :: this !< A reference to this object
      real(8), dimension(:,:), intent(in)  :: dm   !< Design matrix (each row is an x point)
      real(8), dimension(:),   intent(in)  :: fm   !< 'Measures of f'
      real(8), dimension(:),   intent(in)  :: wm   !< Weight of 'Measures of f'
      integer,                 intent(out) :: es   !< Exit status: 0 = success, 1 = failure

      ! Inner variables

      integer :: k, p, q ! Dummy indexes

      real(8), allocatable, dimension(:,:) :: A ! Matrix
      real(8), allocatable, dimension(:)   :: b ! Source


      ! Number of measures
      this%m = size(dm,1)

      ! Number of unknowns
      this%n = size(dm,2)

      ! Number of base functions
      this%nb = (this%n+1) * (this%n+2) / 2

      ! Allocating resources
      if ( allocated(this%cp) ) deallocate(this%cp)

      allocate(this%cp(this%nb))
      allocate(A(this%nb,this%nb))
      allocate(b(this%nb))

      ! Initializing exit status
      es = 0

      ! Calculating matrix coefficients and source

      do p = 1, this%nb

         b(p) = 0.d0

         do k = 1, this%m

            b(p) = b(p) + fm(k) * this%phi( p, dm(k,:) ) * wm(k)

         end do


         do q = 1, this%nb

            A(q,p) = 0.d0

            do k = 1, this%m

               A(q,p) = A(q,p) + this%phi( q, dm(k,:) ) * this%phi( p, dm(k,:) ) * wm(k)

            end do

         end do

      end do

      ! Calculating the coefficients

      call gausstp( this%nb, A, b, this%cp, es)

      deallocate(A)
      deallocate(b)

   end subroutine


   !> \brief Returns the value of the fitted response surface for a given x
   real(8) function P(this, x)
      implicit none
      class(class_RSM_Quadratic_Model)  :: this !< A reference to this object
      real(8), dimension(:), intent(in) :: x    !< Independent variables

      ! Inner variables

      integer :: j  ! Dummy

      P = 0.d0

      do j = 1, this%nb

         P = P + this%cp(j) * this%phi(j, x)

      end do

   end function


   !> \brief Returns the response surface optimizer
   subroutine get_optimizer(this, dm, x, ko, es)
      implicit none
      class(class_RSM_Quadratic_Model)     :: this !< A reference to this object
      real(8), dimension(:,:), intent(in)  ::   dm !< Design matrix (each row is an x point)
      real(8), dimension(:),   intent(out) ::    x !< Coordinates of the optimizer
      integer,                 intent(out) ::   ko !< ko: -1 = minimizer, 0 = saddle point, 1 = maximizer
      integer,                 intent(out) ::   es !< Exit status: 0 = success, 1 = failure


      ! Inner variables
      integer :: i, j, p ! Dummy indexes

      real(8), dimension(this%n,this%n) :: A  ! Coefficients of the optimization matrix
      real(8), dimension(this%n)        :: b  ! Source of the optimization matrix

      associate ( n  => this%n,  &
                  m  => this%m,  &
                  nb => this%nb, &
                  cp => this%cp  )

         ! Calculates the coefficients and source of the optimization matrix

         do i = 1, n

            p = i + n + 1

            A(i,i) = 2.d0 * cp(p)

            do j = i+1, n

               p = j - i + (i-1) * ( 2 * n - i ) / 2 + 2 * n + 1

               A(i,j) = cp(p)

            end do

            p = i + 1

            b(i) = - cp(p)

         end do


         do i = 1, n

            do j = 1, i-1

               A(i,j) = A(j,i)

            end do

         end do

         ! Solving the linear system for optimization
         call gausstp( n, A, b, x, es)

         ! Checking for errors
         if ( es /= 0 ) return

         ! Determines if x is a maximizer, minimizer or saddle point
         call get_kind_of_optimum(m, n, dm, x, Pol, ko)

      end associate

   contains

      real(8) function Pol(x)
         implicit none
         real(8), dimension(:) :: x

         Pol = this%P(x)

      end function

   end subroutine


   !> \brief Base function
   real(8) function phi(this, p, x)
      implicit none
      class(class_RSM_Quadratic_Model)  :: this  !< A reference to this object
      integer,               intent(in) :: p     !< Number of the base function
      real(8), dimension(:), intent(in) :: x     !< Variables

      ! Inner variables

      integer :: i, j ! Dummy index

      associate ( n  => this%n, &
                  nb => this%nb )

         phi = 0.d0

         if ( p == 1 ) then

            phi = 1.d0

         else if ( 2 <= p .and. p <= n+1 ) then

            i = p-1

            phi = x(i)

         else if ( n+2 <= p .and. p <= 2*n+1 ) then

            i = p - n - 1

            phi = x(i) * x(i)

         else if ( 2*n+2 <= p .and. p <= nb ) then

            call get_ij(p,n)

            phi = x(i) * x(j)

         else

            write(*,*) "Function phi: error, out of range."

            call mod_mpi_finalize()

         end if

      end associate

   contains

      !> \brief For a given p, finds the indexes i and j
      subroutine get_ij(p, n)
         implicit none
         integer, intent(in) :: p
         integer, intent(in) :: n

         integer :: p1

         outer: do i = 1, n

            do j = i+1, n

               p1 = j - i + (i-1) * (2*n-i) / 2 + 2 * n + 1

               if ( p1 == p ) exit outer

            end do

         end do outer

      end subroutine

   end function

end module
