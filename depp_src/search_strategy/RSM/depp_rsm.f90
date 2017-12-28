!> \brief Response Surface Methodology
!!
!! A quadratic response surface is fitted from a given data set by the weighted least square method.
!! Then, the extremal point (maximum/minimum) is found analytically.
module rsm

   implicit none

   integer, private :: kfm !< Kind of fitting model (1=incomplete quadratic model, 2=complete quadratic model)
   integer, private :: nb  !< Number of base functions

contains


   !> \brief Sets the parameters of rsm module
   subroutine initialize_rsm_module(its_kfm, n)
      implicit none
      integer, intent(in) :: its_kfm !< Local value of kfm
      integer, intent(in) :: n       !< Number of unknowns

      kfm = its_kfm


      ! Calculating the number of base functions nb according to the fitting model
      select case (kfm)

         case (1) ! Incomplete quadratic model

            nb = 2*n+1

         case (2) ! Complete quadratic model

            nb = (n+1) * (n+2) / 2

         case default

            write(*,*) "Unknown option. kfm = ", kfm
            write(*,*) "Stopping..."
            stop

      end select

   end subroutine initialize_rsm_module


   !> \brief Returns the number of base functions
   integer function get_nb()
      implicit none

      get_nb = nb

   end function get_nb


   !> \brief Calculates the coordinates and value of the optimum solution
   !! given by the fitted function
   subroutine get_optimum_solution(m, n, dm, fm, wm, x, ko, es)
      implicit none
      integer, intent(in) :: m !< Number of 'measures'
      integer, intent(in) :: n !< Number of variables
      real(8), dimension(m,n), intent(in)  :: dm !< Design matrix (each row is an x point)
      real(8), dimension(m),   intent(in)  :: fm !< 'Measures of f'
      real(8), dimension(m),   intent(in)  :: wm !< Weight of 'Measures of f'
      real(8), dimension(n),   intent(out) ::  x !< Coordinates of the optimum solution
      integer, intent(out) :: ko !< ko: -1 = minimizer, 0 = saddle point, 1 = maximizer
      integer, intent(out) :: es !< Exit status: 0 = success, 1 = failure


      ! Inner variables

      real(8), dimension(nb)  :: cp ! Coefficients


      ! Initializing kind of optimum
      ko = 2

      ! Initializing exist status
      es = 0


      ! Calculating the fitting coefficients

      call get_fit_coefficients(m, n, dm, fm, wm, cp, es)

      ! Checking for errors
      if ( es /= 0 ) return



      ! Calculates optimal solution
      call get_opt_x(n, cp, x, es)

      ! Checking for errors
      if ( es /= 0 ) return



      ! Determines if x is a maximizer, minimizer or saddle point
      call get_kind_of_optimum(m, n, dm, cp, x, ko)


   end subroutine get_optimum_solution



   !> \brief Calculates the coefficients c_p that fits the model function
   !!
   !! y(x) = sum( c_p * phi_p(x) , p = 1 .. nb )
   !!
   !! to the function f(x).
   !!
   subroutine get_fit_coefficients(m, n, dm, fm, wm, cp, es)
      implicit none
      integer, intent(in) :: m !< Number of 'measures'
      integer, intent(in) :: n !< Number of variables
      real(8), dimension(m,n), intent(in)  :: dm !< Design matrix (each row is an x point)
      real(8), dimension(m),   intent(in)  :: fm !< 'Measures of f'
      real(8), dimension(m),   intent(in)  :: wm !< Weight of 'Measures of f'
      real(8), dimension(nb),  intent(out) :: cp !< Coefficients
      integer,                 intent(out) :: es !< Exit status: 0 = success, 1 = failure

      ! Inner variables

      integer :: k, p, q ! Dummy indexes

      real(8), dimension(nb,nb) :: A ! Matrix

      real(8), dimension(nb)    :: b ! Source


      ! Initializing exit status

      es = 0


      ! Calculating matrix coefficients and source

      do p = 1, nb

         b(p) = 0.d0

         do k = 1, m

            b(p) = b(p) + fm(k) * phi( p, n, dm(k,:) ) * wm(k)

         end do


         do q = 1, nb

            A(q,p) = 0.d0

            do k = 1, m

               A(q,p) = A(q,p) + phi( q, n, dm(k,:) ) * phi( p, n, dm(k,:) ) * wm(k)

            end do

         end do

      end do

      ! Calculating the coefficients

      call gausstp( nb, A, b, cp, es)


   end subroutine get_fit_coefficients





   !> \brief Calculates optimal solution
   subroutine get_opt_x(n, cp, x, es)
      implicit none
      integer,                intent(in)  :: n  !< Number of variables
      real(8), dimension(nb), intent(in)  :: cp !< Fitting coefficients
      real(8), dimension(n),  intent(out) :: x  !< Optimal solution
      integer,                intent(out) :: es !< Exit status: 0 = success, 1 = failure

      ! Selecting model
      select case (kfm)
         case (1)

            call get_opt_x_iqm(n, cp, x, es)

         case (2)

            call get_opt_x_cqm(n, cp, x, es)

         case default

      end select


   end subroutine get_opt_x



   !> \brief Calculates optimal solution for the incomplete quadratic model
   subroutine get_opt_x_cqm(n, cp, x, es)
      implicit none
      integer,                intent(in)  :: n  !< Number of variables
      real(8), dimension(nb), intent(in)  :: cp !< Fitting coefficients
      real(8), dimension(n),  intent(out) :: x  !< Optimal solution
      integer,                intent(out) :: es !< Exit status: 0 = success, 1 = failure

      ! Inner variables
      real(8), dimension(n,n) :: A  ! Coefficients of the optimization matrix

      real(8), dimension(n)   :: b  ! Source of the optimization matrix


      ! Calculates the coefficients and source of the optimization matrix

      call get_opt_coef_source_cqm(n, cp, A, b)


      ! Solving the linear system for optimization

      call gausstp( n, A, b, x, es)

   end subroutine get_opt_x_cqm



   !> \brief Calculates optimal solution for the incomplete quadratic model
   subroutine get_opt_x_iqm(n, cp, x, es)
      implicit none
      integer,                intent(in)  :: n  !< Number of variables
      real(8), dimension(nb), intent(in)  :: cp !< Fitting coefficients
      real(8), dimension(n),  intent(out) :: x  !< Optimal solution
      integer,                intent(out) :: es !< Exit status: 0 = success, 1 = failure

      ! Inner variables

      integer :: i, p, p1 ! Dummy indexes

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

   end subroutine get_opt_x_iqm





   !> \brief Calculates the coefficients and source of the optimization matrix
   !! for the complete quadratic model
   subroutine get_opt_coef_source_cqm(n, cp, A, b)
      implicit none
      integer, intent(in) :: n !< Number of variables
      real(8), dimension(nb),  intent(in)  :: cp !< Fitting coefficients
      real(8), dimension(n,n), intent(out) :: A  !< Coefficients of the optimization matrix
      real(8), dimension(n),   intent(out) :: b  !< Source of the optimization matrix

      ! Inner variables

      integer :: i, j, p ! Dummy indexes

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

   end subroutine get_opt_coef_source_cqm



   !> \brief Base function
   real(8) function phi( p, n, x)
      implicit none
      integer, intent(in) :: p !< Number of the base function
      integer, intent(in) :: n !< Number of variables
      real(8), dimension(n), intent(in) :: x !< Variables


      ! Selecting model
      select case (kfm)
         case (1)

            phi = phi_iqm( p, n, x)

         case (2)

            phi = phi_cqm( p, n, x)

         case default

            phi = 0.d0

            stop

      end select

   end function phi



   !> \brief Base function of the complete quadratic model
   real(8) function phi_cqm( p, n, x)
      implicit none
      integer, intent(in) :: p !< Number of the base function
      integer, intent(in) :: n !< Number of variables
      real(8), dimension(n), intent(in) :: x !< Variables

      ! Inner variables

      integer :: i, j ! Dummy index

      if ( p == 1 ) then

         phi_cqm = 1.d0

      else if ( 2 <= p .and. p <= n+1 ) then

         i = p-1

         phi_cqm = x(i)

      else if ( n+2 <= p .and. p <= 2*n+1 ) then

         i = p - n - 1

         phi_cqm = x(i) * x(i)

      else if ( 2*n+2 <= p .and. p <= nb ) then

         call get_ij

         phi_cqm = x(i) * x(j)

      else

         write(*,*) "Function phi: error, out of range."

         stop

      end if


   contains

      !> \brief For a given p, finds the indexes i and j
      subroutine get_ij
         implicit none

         integer :: p1

         outer: do i = 1, n

            do j = i+1, n

               p1 = j - i + (i-1) * (2*n-i) / 2 + 2 * n + 1

               if ( p1 == p ) exit outer

            end do

         end do outer

      end subroutine get_ij

   end function phi_cqm


   !> \brief Base function of the incomplete quadratic model
   real(8) function phi_iqm( p, n, x)
      implicit none
      integer, intent(in) :: p !< Number of the base function
      integer, intent(in) :: n !< Number of variables
      real(8), dimension(n), intent(in) :: x !< Variables

      ! Inner variables

      integer ::  i ! Dummy index


      if ( p == 1 ) then

         phi_iqm = 1.d0

      else if ( 2 <= p .and. p <= n+1 ) then

         i = p-1

         phi_iqm = x(i)

      else if ( n+2 <= p .and. p <= 2*n+1 ) then

         i = p - n - 1

         phi_iqm = x(i) * x(i)

      else

         write(*,*) "Function phi: error, out of range."

         stop

      end if

   end function phi_iqm



   !> \brief Calculates the fitted polynomial
   real(8) function Pol(n, cp, x)
      implicit none
      integer, intent(in) :: n !< Number of variables
      real(8), dimension(nb),    intent(in) :: cp !< Coefficients
      real(8), dimension(n),     intent(in) :: x  !< Variables

      ! Inner variables

      integer :: p  ! Dummy

      Pol = 0.d0

      do p = 1, nb

         Pol = Pol + cp(p) * phi(p, n, x)

      end do

   end function Pol



   !> \brief Determines if the optimum is a maximizer, minimizer or a saddle point
   !! based on the points used for fitting
   subroutine get_kind_of_optimum(m, n, dm, cp, x, ko)
      implicit none
      integer, intent(in)  :: m  !< Number of 'measures'
      integer, intent(in)  :: n  !< Number of variables
      real(8), dimension(m,n), intent(in)  :: dm !< Design matrix (each row is an x point)
      real(8), dimension(nb),  intent(in)  :: cp !< Fitting coefficients
      real(8), dimension(n),   intent(in)  :: x  !< Critical point
      integer,                 intent(out) :: ko !< ko: -1 = minimizer, 0 = saddle point, 1 = maximizer

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

      fc = Pol(n, cp, x)


      ! Searching for maximum and minimum

      do k = 1, m

         Pc = Pol(n, cp, dm(k,:))

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

   end subroutine get_kind_of_optimum




   !> \brief Calculates the weights wm
   subroutine get_weights(kw, m, fm, wm)
      implicit none
      integer, intent(in) :: kw !< Kind of weight (1=uniform, 2=exp. decrease from max. f)
      integer, intent(in) :: m  !< Number of 'measures'
      real(8), dimension(m),   intent(in)  :: fm !< 'Measures of f'
      real(8), dimension(m),   intent(out) :: wm !< Weight of 'Measures of f'

      ! Parameters
      real(8), parameter :: tol = sqrt(epsilon(1.d0))

      ! Inner variables
      integer :: k
      integer :: kmax


      select case (kw)

         case (1) ! Uniform weighting

            wm = 1.d0

         case (2) ! Exponential decrease from maximum f

            kmax = maxloc(fm,1)

            if ( abs(fm(kmax)) > tol ) then

               do k = 1, m

                  wm(k) = exp( ( fm(k) - fm(kmax) ) / fm(kmax) )

               end do

            else

               do k = 1, m

                  wm(k) = exp( fm(k) - fm(kmax) )

               end do

            end if

         case default

            wm = 1.d0

      end select

   end subroutine get_weights

   !> \brief Uses gaussian elimination to solve linear systems
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


end module rsm
