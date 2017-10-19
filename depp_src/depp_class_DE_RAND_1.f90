
!> \brief Defines the search strategy DE/rand/1

module mod_class_DE_RAND_1

   use mod_random_generator

   use mod_class_abstract_search_strategy

   implicit none

   ! Makes everything private
   private

   ! Public class defining DE/rand/1 search strategy
   type, public, extends(class_abstract_search_strategy) :: class_DE_RAND_1

      private

      real(8) :: dif !< Differential parameter
      real(8) :: crs !< Crossing over parameter

   contains

      procedure, public, pass :: init
      procedure, public, pass :: get_trial

   end type

contains

   !> \brief Constructor
   subroutine init(this)
      implicit none
      class(class_DE_RAND_1) :: this

      this%dif = 0.85d0
      this%crs = 0.5d0

      print*, "Initializing DE/RAND/1..."

   end subroutine


   !> \brief Generates a trial individual
   subroutine get_trial(this, ind, pop, x)
      implicit none
      class(class_DE_RAND_1)               :: this
      integer,                 intent(in)  :: ind !< number of the individual
      real(8), dimension(:,:), intent(in)  :: pop !< population
      real(8), dimension(:),   intent(out) :: x


      ! Inner variables
      integer :: nu
      integer :: np
      integer :: r(3) ! indexes of selected individuals


      ! Detecting nu and np
      nu = size(x)
      np = size(pop,dim=1)

      ! Choosing three individuals from the population
      call select_individuals(np, ind, r)

      x = pop(r(3),:) + this%dif*(pop(r(1),:) - pop(r(2),:))

      call crossing_over(ind, nu, np, this%crs, pop, x)

   end subroutine



   !> \brief Selects three distinct individuals of the population.
   subroutine select_individuals(np, ind, r)
      implicit none
      integer, intent(in)  :: np    !< population size
      integer, intent(in)  :: ind   !< number of the individual
      integer, intent(out) :: r(3)  !< indexes of selected individuals

      real(8) :: rnd

      r = ind

      do while (r(1) == ind)

         call rand_number(rnd)

         r(1) = int(rnd*np) + 1

      end do

      do while (r(2) == r(1) .or. r(2) == ind)

         call rand_number(rnd)

         r(2) = int(rnd*np) + 1

      end do

      do while (r(3) == r(1) .or. r(3) == r(2) .or. r(3) == ind)

         call rand_number(rnd)

         r(3) = int(rnd*np) + 1

      end do

   end subroutine select_individuals



   !> \brief Performs the crossing over
   subroutine crossing_over(ind, nu, np, crs, pop, x)
      implicit none
      integer, intent(in)    :: ind          !< number of the individual
      integer, intent(in)    :: nu           !< number of unknowns
      integer, intent(in)    :: np           !< population size
      real(8), intent(in)    :: crs          !< crossover constant
      real(8), intent(in)    :: pop(np,nu)   !< population
      real(8), intent(inout) :: x(nu)        !< trial individual

      ! Inner variables
      integer :: j
      integer :: irnd
      real(8) :: rnd


      call rand_number(rnd)

      irnd = int(rnd*nu) + 1

      do j = 1, nu

         call rand_number(rnd)

         if (rnd < crs .or. irnd == j) then

            !x(j) = x(j)

         else

            x(j) = pop(ind,j)

         end if

      end do

   end subroutine crossing_over




end module
