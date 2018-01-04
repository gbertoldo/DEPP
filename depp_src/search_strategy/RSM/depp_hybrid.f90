!> \brief DE-RSM interface.
!!
!! \details This module contains subroutines related to the hybridization
!! of the Differential Evolution algorithm with the Response Surface Methodology.
module hybrid

   use rsm_dynamic_control
   use qsort
   use mod_search_tools
   use mod_class_ifile
   use mod_class_system_variables

   use mod_class_abstract_RSM
   use mod_class_RSM_factory
   use mod_global_parameters

   implicit none

   integer, private :: nf !< Number of points for fitting the objective function to the polynomial
   integer, private :: kw
   integer, private :: nstp
   real(8), private :: crsh
   real(8), private :: netol
   type(class_RSM_factory)            :: RSM_factory
   class(class_abstract_RSM), pointer :: RSM => null()

contains


   !> \brief Initializes hybrid module and checks hybridization necessary condition for RSM
   subroutine initialize_hybrid_module(sys_var, conf_file_name, es)
      implicit none
      class(class_system_variables), intent(in) :: sys_var
      character(len=*), intent(in) :: conf_file_name
      integer, intent(out)  :: es    !< Exit status (0=success, 1=failure)


      integer :: nu    !< Number of unknowns
      integer :: np    !< Size of the population
      integer :: ng    !< Maximum number of generations
      real(8) :: fnb   !< Multiple of the minimum number of points for RSM fitting
      real(8) :: fh    !< Initial hybridization factor
      real(8) :: fhmin !< Minimum hybridization factor
      real(8) :: fhmax !< Maximum hybridization factor
      integer :: fhm   !< Model for the dynamical calculation of the factor of hybridization

      character(str_size) :: RS_model

      type(class_ifile) :: ifile1
      type(class_ifile) :: ifile2

      call ifile1%init(filename=trim(sys_var%absfolderin)//trim(sys_var%parfile), field_separator="&")
      call ifile2%init(filename=conf_file_name, field_separator="&")

      call ifile1%load()
      call ifile2%load()

      call ifile2%get_value(nstp,"nstp")
      call ifile2%get_value(crsh,"crsh")
      call ifile2%get_value(kw,"kw")    !< Kind of weight
      call ifile2%get_value(RS_model,"RS_model")  ! Response Surface model
      call ifile1%get_value(nu,"nu")    !< Number of unknowns
      call ifile1%get_value(np,"np")    !< Size of the population
      call ifile1%get_value(ng,"ng")    !< Maximum number of generations
      call ifile2%get_value(fnb,"fnb")   !< Multiple of the minimum number of points for RSM fitting
      call ifile2%get_value(fh,"fh")    !< Initial hybridization factor
      call ifile2%get_value(fhmin,"fhmin") !< Minimum hybridization factor
      call ifile2%get_value(fhmax,"fhmax") !< Maximum hybridization factor
      call ifile2%get_value(fhm,"fhm")   !< Model for the dynamical calculation of the factor of hybridization
      call ifile2%get_value(netol,"netol")


      ! Initializing RSM Dynamic Control module
      call initialize_rsm_dynamic_control(np, fh, fhmin, fhmax, fhm)


      ! Initializing exit status
      es = 0

      call RSM_factory%create(RS_model, RSM)

      ! Number of points for fitting the objective function to the polynomial
      nf = ceiling( fnb * dble(RSM%dim(nu)) ) + 1

      if ( np * (ng-1) <= 2 * nf ) then

         write(*,*) "ERROR: maximum number of generations ng is insufficient to apply RSM."
         write(*,*) "Minimum ng recommended: ", int(dble(10*nf)/dble(np))+1

         es = 1

      end if

   end subroutine initialize_hybrid_module


   !> \brief Checks if the RSM may be applied
   logical function rsm_check(np, g, fh)
      implicit none
      integer, intent(in)   :: np    !< Size of the population
      integer, intent(in)   :: g     !< Current generation
      real(8), intent(in)   :: fh    !< Fraction of hybridization

      ! Inner variables
      real(8) :: rnd ! Random number

      rsm_check = .false.

      call random_number(rnd)

      if ( associated(RSM) .and. ( 2 * nf <= np * (g-1)) .and. rnd <= fh ) rsm_check = .true.

   end function rsm_check


   !> \brief Returns the best estimate given by the Response Surface Methodology
   subroutine get_rsm_optimum(ind, nu, np, ng, g, xmin, xmax, pop, hist, x, es)
      implicit none
      integer, intent(in) :: ind   !< Current individual
      integer, intent(in) :: nu    !< Dimension of the problem
      integer, intent(in) :: np    !< Size of the population
      integer, intent(in) :: ng    !< Maximum number of generations
      integer, intent(in) :: g     !< Current generation
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
      !call get_optimum_solution(nf, nu, dm, fm, wm, x, ko, es)

      call RSM%fit(dm, fm, wm, es)

      ! Checking for errors
      if ( es /= 0 ) return

      call RSM%get_optimizer(dm, x, ko, es)

      ! Checking for errors
      if ( es /= 0 ) return


      ! kind of optimizer: -1 = minimizer, 0 = saddle point, 1 = maximizer
      if ( ko /= 1 ) then

         ! If ko /= 1 (not a maximizer), an error is returned

         es = 1

         return

      end if

      ! Checking for errors
      if ( es /= 0 ) return


      ! Crossing over
      if ( crsh >= 0.d0 ) call crossing_over(ind, nu, np, crsh, pop, x)


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



end module hybrid
