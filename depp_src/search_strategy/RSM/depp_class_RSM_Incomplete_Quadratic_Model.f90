!> \brief Provides a class for fitting an incomplete quadratic response surface and returning its optimizer

module mod_class_RSM_Incomplete_Quadratic_Model

   use mod_class_abstract_RSM
   use mod_gauss_solver

   implicit none

   !> \brief Incomplete quadratic response surface model
   type, public, extends(class_abstract_RSM) :: class_RSM_Incomplete_Quadratic_Model

      private

      integer                             :: nb !< Number of base functions
      integer                             :: m  !< Number of 'measures'
      integer                             :: n  !< Number of unknowns
      real(8), allocatable, dimension(:)  :: cp !< Coefficients

   contains

      procedure, public,  pass :: dim                  !< Minimum number of points necessary to fit the response surface
      procedure, public,  pass :: fit                  !< Fits the polynomial to the data
      procedure, public,  pass :: P                    !< Returns the value of the fitted response surface for a given x
      procedure, public,  pass :: get_optimizer        !< Returns the response surface optimizer

      procedure, private, pass :: phi                  !< Base function
      procedure, private, pass :: get_kind_of_optimum  !< Determines if the critical point is a maximizer, minimizer or saddle point

   end type

contains


   !> \brief Minimum number of points necessary to fit the response surface
   integer function dim(this, n)
      implicit none
      class(class_RSM_Incomplete_Quadratic_Model) :: this
      integer, optional, intent(in)               :: n

      if ( present(n) ) then

         dim = 2*n+1

      else

         dim = this%nb

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
      class(class_RSM_Incomplete_Quadratic_Model) :: this
      real(8), dimension(:,:),        intent(in)  :: dm   !< Design matrix (each row is an x point)
      real(8), dimension(:),          intent(in)  :: fm   !< 'Measures of f'
      real(8), dimension(:),          intent(in)  :: wm   !< Weight of 'Measures of f'
      integer,                        intent(out) :: es   !< Exit status: 0 = success, 1 = failure

      ! Inner variables

      integer :: k, p, q ! Dummy indexes

      real(8), allocatable, dimension(:,:) :: A ! Matrix
      real(8), allocatable, dimension(:)   :: b ! Source


      ! Number of measures
      this%m = size(dm,1)

      ! Number of unknowns
      this%n = size(dm,2)

      ! Number of base functions
      this%nb = 2*this%n+1

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
      class(class_RSM_Incomplete_Quadratic_Model)  :: this
      real(8), dimension(:),            intent(in) :: x    !< Independent variables

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
      class(class_RSM_Incomplete_Quadratic_Model) :: this
      real(8), dimension(:,:),        intent(in)  ::   dm !< Design matrix (each row is an x point)
      real(8), dimension(:),          intent(out) ::    x !< Coordinates of the optimizer
      integer,                        intent(out) ::   ko !< ko: -1 = minimizer, 0 = saddle point, 1 = maximizer
      integer,                        intent(out) ::   es !< Exit status: 0 = success, 1 = failure


      ! Inner variables
      integer :: i, p, p1 ! Dummy indexes

      associate ( n  => this%n,  &
                  m  => this%m,  &
                  nb => this%nb, &
                  cp => this%cp  )


         ! Initializing the exit status
         es = 0

         do i = 1, n

            p  = i + n + 1

            p1 = i + 1

            if ( abs(cp(p)) < 10.d0 * epsilon(-1.d0) ) then

               es = 1

               return

            end if

            x(i) = - cp(p1) / ( 2.d0 * cp(p) )

         end do

         ! Determines if x is a maximizer, minimizer or saddle point
         call this%get_kind_of_optimum(m, n, dm, x, ko)

      end associate

   end subroutine


   !> \brief Base function
   real(8) function phi(this, p, x)
      implicit none
      class(class_RSM_Incomplete_Quadratic_Model)  :: this
      integer,                          intent(in) :: p     !< Number of the base function
      real(8), dimension(:),            intent(in) :: x     !< Variables

      ! Inner variables

      integer :: i ! Dummy index

      associate ( n  => this%n )

         if ( p == 1 ) then

            phi = 1.d0

         else if ( 2 <= p .and. p <= n+1 ) then

            i = p-1

            phi = x(i)

         else if ( n+2 <= p .and. p <= 2*n+1 ) then

            i = p - n - 1

            phi = x(i) * x(i)

         else

            write(*,*) "Function phi: error, out of range."

            stop

         end if

      end associate

   end function


   !> \brief Determines if the optimum is a maximizer, minimizer or a saddle point
   !! based on the points used for fitting
   subroutine get_kind_of_optimum(this, m, n, dm, x, ko)
      implicit none
      class(class_RSM_Incomplete_Quadratic_Model) :: this
      integer,                        intent(in)  :: m    !< Number of 'measures'
      integer,                        intent(in)  :: n    !< Number of variables
      real(8), dimension(m,n),        intent(in)  :: dm   !< Design matrix (each row is an x point)
      real(8), dimension(n),          intent(in)  :: x    !< Critical point
      integer,                        intent(out) :: ko   !< ko: -1 = minimizer, 0 = saddle point, 1 = maximizer

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

      fc = this%P(x)


      ! Searching for maximum and minimum

      do k = 1, m

         Pc = this%P(dm(k,:))

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
