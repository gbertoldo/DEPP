
!> \brief Provides a class that calculates the trial individuals based on the Response
!! Surface Methodology

module mod_class_RSM_search_strategy

   use mod_mpi
   use qsort
   use mod_search_tools
   use mod_class_ifile
   use mod_class_system_variables
   use mod_class_abstract_search_strategy
   use mod_class_abstract_RSM
   use mod_class_RSM_factory
   use mod_string
   use mod_class_ehist
   use mod_random_generator

   implicit none

   ! Makes everything private, except otherwise stated
   private

   !> \brief Search strategy base on the Response Surface Methodology
   type, public, extends(class_abstract_search_strategy) :: class_RSM_search_strategy

      private

      integer                            :: np            !< Population size
      integer                            :: nf            !< Number of fitting points
      integer                            :: kw            !< Kind of weight
      real(8)                            :: crsh          !< Crossing over parameter
      real(8)                            :: netol         !< Tolerance for selecting neighbor points
      class(class_abstract_RSM), pointer :: RSM => null() !< Response surface

   contains

      procedure, pass, public  :: init                       !< Constructor
      procedure, pass, public  :: get_trial                  !< Returns the trial individual (best estimate given by the Response Surface Methodology)
      procedure, pass, public  :: feed_back                  !< Process the fitness of the trial individual
      procedure, pass, public  :: data_size                  !< Gives the size of the shared data vector
      procedure, pass, public  :: send                       !< Send data to other threads
      procedure, pass, public  :: recv                       !< Receive data from other threads
      procedure, pass, public  :: update                     !< Perform update calculations after parallel computation cycle
      procedure, pass, private :: select_target_individual   !< Selects a target individual from the history list
      procedure, pass, private :: select_fitting_individuals !< Selects the fitting individuals for a given target individual
      procedure, pass, private :: get_weights                !< Calculates the weights of the fitting individuals
      procedure, pass, public  :: get_nf                     !< Returns the number of points expected for fitting the response surface

   end type

contains

   !> \brief Constructor
   subroutine init(this, sys_var, conf_file_name)
      implicit none
      class(class_RSM_search_strategy)             :: this           !< A reference to this object
      class(class_system_variables),   intent(in)  :: sys_var        !< System's variables
      character(len=*),                intent(in)  :: conf_file_name !< Name of the configuration file

      ! Inner variables

      integer                 :: nu          ! Number of unknowns
      integer                 :: ng          ! Maximum number of generations
      real(8)                 :: fnb         ! Multiple of the minimum number of points for RSM fitting
      character(str_size)     :: RS_model    ! Response surface model
      type(class_ifile)       :: ifile1      ! Input file 1
      type(class_ifile)       :: ifile2      ! Input file 2
      type(class_RSM_factory) :: RSM_factory ! Response surface factory



      ! Reading input files

      call ifile1%init(filename=trim(sys_var%absfolderin)//trim(sys_var%parfile), field_separator="&")
      call ifile1%load()
      call ifile1%get_value(nu,"nu")         ! Number of unknowns
      call ifile1%get_value(this%np,"np")    ! Size of the population
      call ifile1%get_value(ng,"ng")         ! Maximum number of generations

      call ifile2%init(filename=conf_file_name, field_separator="&")
      call ifile2%load()
      call ifile2%get_value( this%crsh,     "crsh")  ! Crossing over parameter
      call ifile2%get_value(   this%kw,       "kw")  ! Kind of weight
      call ifile2%get_value(  RS_model, "RS_model")  ! Response Surface model
      call ifile2%get_value(       fnb,      "fnb")  ! Multiple of the minimum number of points for RSM fitting
      call ifile2%get_value(this%netol,    "netol")  ! Tolerance for selecting neighbor points


      ! Creating response surface model
      call RSM_factory%create(sys_var, RS_model, this%RSM)


      ! Number of points for fitting the objective function to the polynomial
      this%nf = ceiling( fnb * dble(this%RSM%nfit(nu)) ) + 1


      ! Checking if the number of generations is enough to apply RSM
      if ( this%np * (ng-1) <= 2 * this%nf ) then

         call sys_var%logger%println("ERROR: maximum number of generations ng is insufficient to apply RSM.")
         call sys_var%logger%println("Minimum ng recommended: " // to_string(int(dble(10*this%nf)/dble(this%np))+1))

         call mod_mpi_finalize()

      end if

   end subroutine


   !> \brief Returns the trial individual (best estimate given by the Response Surface Methodology)
   subroutine get_trial(this, ind, ehist, x, es)
      implicit none
      class(class_RSM_search_strategy)      :: this  !< A reference to this object
      integer,                  intent(in)  :: ind   !< Current individual
      class(class_ehist),       intent(in)  :: ehist !< Evolution history
      real(8), dimension(:),    intent(out) :: x     !< Best estimated individual
      integer, optional,        intent(out) :: es    !< Exit status: 0 = success, 1 = failure


      ! Inner variables

      integer :: ko    ! kind of optimizer: -1 = minimizer, 0 = saddle point, 1 = maximizer

      real(8), dimension(this%nf, ehist%nu) :: dm    ! Design matrix
      real(8), dimension(this%nf)           :: fm    ! f
      real(8), dimension(this%nf)           :: wm    ! Weight of f
      real(8), dimension(ehist%nu)          :: xs    ! Selected individual



      ! Initializing exit status
      es = 0



      ! Selects a target individual from the history list
      call this%select_target_individual(ind, ehist%nu, ehist%np, ehist%ng, ehist%g, ehist%hist, xs)



      ! Selects the fitting individuals for a given target individual
      call this%select_fitting_individuals(ehist%nu, ehist%np, ehist%ng, ehist%g, this%netol, &
                                           ehist%hist, xs, ehist%xmin, ehist%xmax, dm, fm, es)

      ! Checking for errors
      if ( es /= 0 ) return


      ! Calculates the weights wm
      call this%get_weights(this%kw, this%nf, fm, wm)


      ! Searching for the best solution
      call this%RSM%fit(dm, fm, wm, es)


      ! Checking for errors
      if ( es /= 0 ) return


      call this%RSM%get_optimizer(dm, x, ko, es)


      ! Checking for errors
      if ( es /= 0 ) return


      ! kind of optimizer: -1 = minimizer, 0 = saddle point, 1 = maximizer
      if ( ko /= 1 ) then

         ! If ko /= 1 (not a maximizer), an error is returned

         es = 1

         return

      end if

      ! Crossing over
      if ( this%crsh >= 0.d0 ) call crossing_over(ind, ehist%nu, ehist%np, this%crsh, ehist%pop, x)


   end subroutine


   !> \brief Process the fitness of the trial individual.
   subroutine feed_back(this, ind, ehist, fit, ecode)
      implicit none
      class(class_RSM_search_strategy)      :: this    !< A reference to this object
      integer,                  intent(in)  :: ind     !< Number of the individual of the population
      class(class_ehist),       intent(in)  :: ehist   !< Evolution history
      real(8),                  intent(in)  :: fit     !< Fitness of the trial individual
      integer,                  intent(in)  :: ecode   !< Error code

   end subroutine


  !> \brief Gives the size of the shared data vector
  integer function data_size(this)
     implicit none
     class(class_RSM_search_strategy) :: this !< A reference to this object

     data_size = this%np

  end function


   !> \brief Send data to other threads
   subroutine send(this, i, to_thread)
      implicit none
      class(class_RSM_search_strategy)      :: this      !< A reference to this object
      integer,                   intent(in) :: i         !< Index of the shared data
      integer,                   intent(in) :: to_thread !< Receiver thread

   end subroutine


   !> \brief Receive data from other threads
   subroutine recv(this, i, from_thread)
      implicit none
      class(class_RSM_search_strategy)      :: this        !< A reference to this object
      integer,                   intent(in) :: i           !< Index of the shared data
      integer,                   intent(in) :: from_thread !< Sender thread

   end subroutine


   !> \brief Perform update calculations after parallel computation cycle
   subroutine update(this)
      implicit none
      class(class_RSM_search_strategy) :: this !< A reference to this object

   end subroutine

   !> \brief Selects a target individual from the history list
   subroutine select_target_individual(this, ind, nu, np, ng, g, hist, xs)
      implicit none
      class(class_RSM_search_strategy)            :: this !< A reference to this object
      integer,                        intent(in)  :: ind  !< Current individual
      integer,                        intent(in)  :: nu   !< Dimension of the problem
      integer,                        intent(in)  :: np   !< Size of the population
      integer,                        intent(in)  :: ng   !< Number of generations
      integer,                        intent(in)  :: g    !< Current generation
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
   subroutine select_fitting_individuals(this, nu, np, ng, g, netol, hist, xs, xmin, xmax, dm, fm, es)
      implicit none
      class(class_RSM_search_strategy)             :: this !< A reference to this object
      integer,                         intent(in)  :: nu    !< Dimension of the problem
      integer,                         intent(in)  :: np    !< Size of the population
      integer,                         intent(in)  :: ng    !< Number of generations
      integer,                         intent(in)  :: g     !< Current generation
      real(8),                         intent(in)  :: netol !< Tolerance for distance when selecting neighbors points
      real(8), dimension(ng,np,0:nu),  intent(in)  :: hist  !< History up to previous generation
      real(8), dimension(nu),          intent(in)  :: xs    !< Given individual
      real(8), dimension(nu),          intent(in)  :: xmin  !< Lower bound of the domain of optimization
      real(8), dimension(nu),          intent(in)  :: xmax  !< Upper bound of the domain of optimization
      real(8), dimension(this%nf, nu), intent(out) :: dm    !< Design matrix
      real(8), dimension(this%nf),     intent(out) :: fm    !< f
      integer,                         intent(out) :: es    !< Exit status: 0 = success, 1 = failure


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

         call rand_number(rnd)

         if ( norm2(xa) > netol .and. rnd < nep ) then

            i = i + 1

            dm(i,1:nu) = histl(ox(j),1:nu)

            fm(i)      = histl(ox(j),0)

         end if

         if ( i == this%nf ) exit

      end do

      if ( i < this%nf ) then

         es = 1

         return

      end if

      ! Deallocating memory
      deallocate(histl)
      deallocate(dist)
      deallocate(ox)

   end subroutine select_fitting_individuals



   !> \brief Calculates the weights of the fitting individuals
   subroutine get_weights(this, kw, m, fm, wm)
      implicit none
      class(class_RSM_search_strategy)     :: this !< A reference to this object
      integer,                 intent(in)  :: kw   !< Kind of weight (1=uniform, 2=exp. decrease from max. f)
      integer,                 intent(in)  :: m    !< Number of 'measures'
      real(8), dimension(m),   intent(in)  :: fm   !< 'Measures of f'
      real(8), dimension(m),   intent(out) :: wm   !< Weight of 'Measures of f'

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


   !> \brief Returns the number of points expected for fitting the response surface
   integer function get_nf(this)
      implicit none
      class(class_RSM_search_strategy) :: this !< A reference to this object

      get_nf = this%nf

   end function


end module
