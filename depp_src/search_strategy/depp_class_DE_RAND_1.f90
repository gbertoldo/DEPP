
!> \brief Defines the search strategy DE/rand/1

module mod_class_DE_RAND_1

   use mod_random_generator
   use mod_class_abstract_search_strategy
   use mod_class_ehist

   implicit none

   ! Makes everything private, except otherwise stated
   private

   ! Public class defining DE/rand/1 search strategy
   type, public, extends(class_abstract_search_strategy) :: class_DE_RAND_1

      private

      real(8) :: dif !< Differential parameter
      real(8) :: crs !< Crossing over parameter

   contains

      procedure, public, pass :: init
      procedure, public, pass :: get_trial
      procedure, public, pass :: feed_back

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
   subroutine get_trial(this, ind, ehist, x, estatus)
      implicit none
      class(class_DE_RAND_1)                :: this
      integer,                  intent(in)  :: ind   ! Number of the individual of the population
      class(class_ehist),       intent(in)  :: ehist ! Evolution history
      real(8), dimension(:),    intent(out) :: x     ! Trial individual
      integer,                  intent(out) :: estatus ! to be removed


      ! Inner variables
      integer :: nu
      integer :: np
      integer :: r(3) ! indexes of selected individuals


      ! Detecting nu and np
      nu = size(x)
      np = size(ehist%pop,dim=1)

      ! Choosing three individuals from the population
      call select_individuals(np, ind, r)

      x = ehist%pop(r(3),:) + this%dif*(ehist%pop(r(1),:) - ehist%pop(r(2),:))

      call crossing_over(ind, nu, np, this%crs, ehist%pop, x)

   end subroutine


   subroutine feed_back(this, ind, ehist, fit, ecode)
      implicit none
      class(class_DE_RAND_1)                :: this
      integer,                  intent(in)  :: ind     ! Number of the individual of the population
      class(class_ehist),       intent(in)  :: ehist   ! Evolution history
      real(8),                  intent(in)  :: fit     ! Fitness of the trial individual
      integer,                  intent(in)  :: ecode   ! Error code

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
