!> \brief DE-RSM interface.
!!
!! \details This module contains subroutines related to the hybridization
!! of the Differential Evolution algorithm with the Response Surface Methodology.
module hybrid

   use rsm
   use rsm_dynamic_control
   use qsort
   use tools

   implicit none

   integer, private :: nf !< Number of points for fitting the objective function to the polynomial

contains


   !> \brief Initializes hybrid module and checks hybridization necessary condition for RSM
   subroutine initialize_hybrid_module(kh, nu, np, ng, fnb, fh, fhm, es)
      implicit none
      integer, intent(in)   :: kh    !< Kind of hybridization (see input file)
      integer, intent(in)   :: nu    !< Number of unknowns
      integer, intent(in)   :: np    !< Size of the population
      integer, intent(in)   :: ng    !< Maximum number of generations
      real(8), intent(in)   :: fnb   !< Multiple of the minimum number of points for RSM fitting
      real(8), intent(in)   :: fh    !< Initial hybridization factor
      integer, intent(in)   :: fhm   !< Model for the dynamical calculation of the factor of hybridization
      integer, intent(out)  :: es    !< Exit status (0=success, 1=failure)


      ! Initializing RSM Dynamic Control module
      call initialize_rsm_dynamic_control(np, fh, fhm)


      ! Initializing exit status
      es = 0


      if ( kh > 0 ) then

         select case (kh)

            case (1) ! Incomplete quadratic model

               ! Number of points for fitting the objective function to the polynomial
               nf = ceiling( fnb * dble(2*nu+1) ) + 1

               ! Sets the parameters of rsm module
               call initialize_rsm_module(1, nu)

            case (2) ! Complete quadratic model

               ! Number of points for fitting the objective function to the polynomial
               nf = ceiling( fnb * dble((nu+1)*(nu+2) / 2) ) + 1

               ! Sets the parameters of rsm module
               call initialize_rsm_module(2, nu)

            case default

               write(*,*) "ERROR: invalid kfm option."

               nf = 0

               es = 1

         end select


         if ( np * (ng-1) <= 2 * nf ) then

            write(*,*) "ERROR: maximum number of generations ng is insufficient to apply RSM."
            write(*,*) "Minimum ng recommended: ", int(dble(10*nf)/dble(np))+1

            es = 1

         end if


      end if


   end subroutine initialize_hybrid_module


   !> \brief Checks if the RSM may be applied
   logical function rsm_check(kh, np, g, fh)
      implicit none
      integer, intent(in)   :: kh    !< Kind of hybridization (see input file)
      integer, intent(in)   :: np    !< Size of the population
      integer, intent(in)   :: g     !< Current generation
      real(8), intent(in)   :: fh    !< Fraction of hybridization

      ! Inner variables
      real(8) :: rnd ! Random number

      rsm_check = .false.

      call random_number(rnd)

      if ( 0 < kh .and. ( 2 * nf <= np * (g-1)) .and. rnd <= fh ) rsm_check = .true.

   end function rsm_check


   !> \brief Returns the best estimate given by the Response Surface Methodology
   subroutine get_rsm_optimum(ind, kw, nu, np, ng, g, crsh, nstp, netol, xmin, xmax, pop, hist, x, es)
      implicit none
      integer, intent(in) :: ind   !< Current individual
      integer, intent(in) :: kw    !< Kind of weighting function
      integer, intent(in) :: nu    !< Dimension of the problem
      integer, intent(in) :: np    !< Size of the population
      integer, intent(in) :: ng    !< Maximum number of generations
      integer, intent(in) :: g     !< Current generation
      real(8), intent(in) :: crsh  !< crossover constant of the hybridized model
      integer, intent(in) :: nstp  !< Number of trials for adjusting the step
      real(8), intent(in) :: netol !< Tolerance for distance when selecting neighbors points
      real(8), dimension(nu),         intent(in)  :: xmin !< Lower bound of the domain of optimization
      real(8), dimension(nu),         intent(in)  :: xmax !< Upper bound of the domain of optimization
      real(8), dimension(np,nu),      intent(in)  :: pop  !< Population
      real(8), dimension(ng,np,0:nu), intent(in)  :: hist !< History up to previous generation
      real(8), dimension(nu),         intent(out) :: x    !< Best estimated individual
      integer,                        intent(out) :: es   !< Exit status: 0 = success, 1 = failure


      ! Inner variables

      integer :: ko    ! kind of optimizer: -1 = minimizer, 0 = saddle point, 1 = maximizer

      real(8), dimension(nf, nu) :: dm    ! Design matrix
      real(8), dimension(nf)     :: fm    ! f
      real(8), dimension(nf)     :: wm    ! Weight of f
      real(8), dimension(nu)     :: xs    ! Selected individual


      ! Initializing exit status
      es = 0



      ! Selects a target individual from the history list
      call select_target_individual(ind, nu, np, ng, g, hist, xs)



      ! Selects the fitting individuals for a given target individual
      call select_fitting_individuals(nu, np, ng, g, netol, hist, xs, xmin, xmax, dm, fm, es)

      ! Checking for errors
      if ( es /= 0 ) return




      ! Calculates the weights wm
      call get_weights(kw, nf, fm, wm)



      ! Searching for the best solution
      call get_optimum_solution(nf, nu, dm, fm, wm, x, ko, es)


      ! Checking for errors
      if ( es /= 0 ) return


      ! kind of optimizer: -1 = minimizer, 0 = saddle point, 1 = maximizer
      if ( ko /= 1 ) then

         ! If ko /= 1 (not a maximizer), an error is returned

         es = 1

         return

      end if




      ! Adjusting the step of the current best individual of dm to the rsm individual x
      call adjusts_rsm_step(kw, nu, np, ng, g, nstp, netol, xmin, xmax, dm, fm, hist, x, es)


      ! Checking for errors
      if ( es /= 0 ) return




      ! Crossing over
      call crossing_over(ind, nu, np, crsh, pop, x)



   end subroutine get_rsm_optimum




   !> \brief Selects a target individual from the history list
   subroutine select_target_individual(ind, nu, np, ng, g, hist, xs)
      implicit none
      integer, intent(in) :: ind   !< Current individual
      integer, intent(in) :: nu    !< Dimension of the problem
      integer, intent(in) :: np    !< Size of the population
      integer, intent(in) :: ng    !< Number of generations
      integer, intent(in) :: g     !< Current generation
      real(8), dimension(ng,np,0:nu), intent(in)  :: hist !< History up to previous generation
      real(8), dimension(nu),         intent(out) :: xs   !< Selected individual

      ! Inner variables

      integer :: i    ! Dummy
      integer :: j    ! Dummy
      integer :: k    ! Dummy
      integer :: ni   ! Number of available individuals in the history list
      integer :: gmax ! Maximum generation of history

      real(8), allocatable, dimension(:,:) :: histl ! History list
      real(8), allocatable, dimension(:)   :: fitl  ! Fitness list
      integer, allocatable, dimension(:)   :: ox    ! Ordering vector


      ! Calculating gmax
      gmax = g-1


      ! Calculating the number of available individuals in the history list
      ni = gmax * np


      ! Allocating memory
      allocate(histl(ni,0:nu))
      allocate(fitl(ni))
      allocate(ox(ni))


      do i = 1, gmax

         do j = 1, np

            k = (i-1) * np + j

            histl(k,:) = hist(i,j,:)

            fitl(k) = -hist(i,j,0)

            ox(k) = k

         end do

      end do


      ! Selecting the ind-th best individual

      call qsort2(fitl,ox)

      xs = histl(ox(ind),1:nu)



      ! Deallocating memory
      deallocate(histl)
      deallocate(fitl)
      deallocate(ox)

   end subroutine select_target_individual




   !> \brief Selects the fitting individuals for a given target individual
   subroutine select_fitting_individuals(nu, np, ng, g, netol, hist, xs, xmin, xmax, dm, fm, es)
      implicit none
      integer, intent(in) :: nu    !< Dimension of the problem
      integer, intent(in) :: np    !< Size of the population
      integer, intent(in) :: ng    !< Number of generations
      integer, intent(in) :: g     !< Current generation
      real(8), intent(in) :: netol !< Tolerance for distance when selecting neighbors points
      real(8), dimension(ng,np,0:nu), intent(in)  :: hist !< History up to previous generation
      real(8), dimension(nu),         intent(in)  :: xs   !< Given individual
      real(8), dimension(nu),         intent(in)  :: xmin !< Lower bound of the domain of optimization
      real(8), dimension(nu),         intent(in)  :: xmax !< Upper bound of the domain of optimization
      real(8), dimension(nf, nu),     intent(out) :: dm   !< Design matrix
      real(8), dimension(nf),         intent(out) :: fm   !< f
      integer,                        intent(out) :: es   !< Exit status: 0 = success, 1 = failure


      ! Parameter
      real(8), parameter :: nep = 0.5d0 ! Probability for selecting a neighbor individual


      ! Inner variables

      integer :: i    ! Dummy
      integer :: j    ! Dummy
      integer :: k    ! Dummy
      integer :: ni   ! Number of available individuals in the history list
      integer :: gmax ! Maximum generation of history
      real(8) :: rnd  ! A random number

      real(8), dimension(nu) :: xa    ! Auxiliary individual

      real(8), allocatable, dimension(:,:) :: histl ! History list
      real(8), allocatable, dimension(:)   :: dist  ! Distance of individuals to the given one
      integer, allocatable, dimension(:)   :: ox    ! Ordering vector


      ! Initializing exit status
      es = 0


      ! Calculating gmax
      gmax = g-1


      ! Calculating the number of available individuals in the history list
      ni = gmax * np


      ! Allocating memory
      allocate(histl(ni,0:nu))
      allocate(dist(ni))
      allocate(ox(ni))



      do i = 1, gmax

         do j = 1, np

            k = (i-1) * np + j

            histl(k,:) = hist(i,j,:)

         end do

      end do


      ! Calculating distances to the selected individual

      do i = 1, ni

         ox(i) = i

         xa = xs - histl(i,1:nu)

         dist(i) = norm2(xa)

      end do


      ! Sorting individuals according to their distance to the selected one

      call qsort2(dist,ox)




      ! Selecting individuals for RSM

      i = 1

      dm(i,1:nu) = histl(ox(i),1:nu)

      fm(i)      = histl(ox(i),0)

      do j = 2, ni

         xa = (histl(ox(j),1:nu)-histl(ox(i),1:nu))/(xmax-xmin)

         call random_number(rnd)

         if ( norm2(xa) > netol .and. rnd < nep ) then

            i = i + 1

            dm(i,1:nu) = histl(ox(j),1:nu)

            fm(i)      = histl(ox(j),0)

         end if

         if ( i == nf ) exit

      end do

      if ( i < nf ) then

         es = 1

         return

      end if

      ! Deallocating memory
      deallocate(histl)
      deallocate(dist)
      deallocate(ox)

   end subroutine select_fitting_individuals




   !> \brief If the RSM optimizer is out of the fitting domain, the estimated
   !! value of the objective function is evaluated and a new optimizer may be
   !! calculated
   subroutine adjusts_rsm_step(kw, nu, np, ng, g, nstp, netol, xmin, xmax, dm, fm, hist, x, es)
      implicit none
      integer, intent(in) :: kw    !< Kind of weighting function
      integer, intent(in) :: nu    !< Number of variables
      integer, intent(in) :: np    !< Size of the population
      integer, intent(in) :: ng    !< Number of generations
      integer, intent(in) :: g     !< Current generation
      integer, intent(in) :: nstp  !< Number of trials for adjusting the step
      real(8), intent(in) :: netol !< Tolerance for distance when selecting neighbors points
      real(8), dimension(nu),         intent(in)    :: xmin  !< Lower bound of the domain of optimization
      real(8), dimension(nu),         intent(in)    :: xmax  !< Upper bound of the domain of optimization
      real(8), dimension(nf,nu),      intent(in)    :: dm    !< Design matrix (each row is an x point)
      real(8), dimension(nf),         intent(in)    :: fm    !< 'Measures of f'
      real(8), dimension(ng,np,0:nu), intent(in)    :: hist  !< History up to previous generation
      real(8), dimension(nu),         intent(inout) :: x     !< Coordinates of the optimizer
      integer,                        intent(out)   :: es    !< Exit status: 0 = success, 1 = failure


      ! Inner variables
      integer :: i      ! Dummy
      integer :: dtag   ! Tag
      real(8) :: f0     ! Maximum value of f in the fitting domain
      real(8) :: x0(nu) ! Maximizer of f in the fitting domain
      real(8) :: Px     ! Estimated value of f(x)


      ! Initializing exit status

      es = 0


      ! Checking the number of trials

      if (nstp == 0) then

         return

      end if


      ! Getting x0 and f0

      f0 = maxval(fm)

      x0 = dm(maxloc(fm,1),:)


      ! Checking if x is in the fitting domain

      dtag = 0

      do i = 1, nu

         if ( x(i) < minval(dm(:,i)) .or. maxval(dm(:,i)) < x(i) ) dtag = 1 ! Out of range!

      end do




      ! If outside the fitting domain, adjusts rsm step

      if ( dtag == 0 ) then ! Inside the fitting domain. Returning...

         return

      else ! Outside the fitting domain. Adjusting rsm step...


         ! Trunking x into domain of optimization

         do i = 1, nu

            if ( x(i) < xmin(i) ) x(i) = xmin(i)

            if ( xmax(i) < x(i) ) x(i) = xmax(i)

         end do



         do i = 1, nstp

            call fit_pol(kw, nu, np, ng, g, netol, hist, xmin, xmax, x, Px, es)


            if ( f0 <= Px .and. es == 0 ) then

               es = 0

               return

            else

               es = 1

               x = ( x + x0 ) / 2.d0

            end if

         end do

      end if

   end subroutine adjusts_rsm_step




   !> \brief Fits the polynomial model near the optimizer and returns its fitted objective function
   subroutine fit_pol(kw, nu, np, ng, g, netol, hist, xmin, xmax, x, Px, es)
      implicit none
      integer, intent(in) :: kw    !< Kind of weighting function
      integer, intent(in) :: nu    !< Number of variables
      integer, intent(in) :: np    !< Size of the population
      integer, intent(in) :: ng    !< Number of generations
      integer, intent(in) :: g     !< Current generation
      real(8), intent(in) :: netol !< Tolerance for distance when selecting neighbors points
      real(8), dimension(ng,np,0:nu), intent(in)  :: hist !< History up to previous generation
      real(8), dimension(nu),         intent(in)  :: xmin !< Lower bound of the domain of optimization
      real(8), dimension(nu),         intent(in)  :: xmax !< Upper bound of the domain of optimization
      real(8), dimension(nu),         intent(in)  :: x    !< Coordinates of the optimizer
      real(8),                        intent(out) :: Px   !< Estimated value of the objective function
      integer,                        intent(out) :: es   !< Exit status: 0 = success, 1 = failure


      ! Inner variables

      real(8), dimension(nf,nu)    :: dm   ! Design matrix (each row is an x point)
      real(8), dimension(nf)       :: fm   ! 'Measures of f'
      real(8), dimension(nf)       :: wm   ! Weighting function

      real(8), allocatable, dimension(:) :: cp ! Fitting coefficients


      ! Allocating cp

      allocate( cp(get_nb()))


      ! Initializing es

      es = 1


      ! Selects the fitting individuals for a given target individual
      call select_fitting_individuals(nu, np, ng, g, netol, hist, x, xmin, xmax, dm, fm, es)


      ! Checking for errors
      if ( es /= 0 ) return


      ! Calculates the weights wm
      call get_weights(kw, nf, fm, wm)



      ! Fitting coefficients
      call get_fit_coefficients(nf, nu, dm, fm, wm, cp, es)


      ! Checking for errors
      if ( es /= 0 ) return


      ! Calculating the estimated value of the objective function
      Px = Pol(nu, cp, x)


      ! Deallocating cp

      deallocate( cp )

   end subroutine fit_pol


end module hybrid
