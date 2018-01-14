
!> \brief Provides a population initializer that generates trial individuals
!! selected randomly, with uniform a distribution, from the search domain.

module mod_class_population_initializer_uniform_random

   use mod_class_abstract_population_initializer
   use mod_class_ehist
   use mod_search_tools

   implicit none

   ! Makes everything private, except otherwise stated
   private

   !> \brief Population initializer class that generates trial individuals
   !! selected randomly, with uniform a distribution, from the search domain.
   type, extends(class_abstract_population_initializer), public :: class_population_initializer_uniform_random

   contains

      procedure, pass, public :: get_trial !< Generates a trial individual

   end type

contains

   !> \brief Gets a trial individual
   subroutine get_trial(this, ind, ehist, x, es)
      implicit none
      class(class_population_initializer_uniform_random) :: this  !< A reference to this object
      integer,                               intent(in)  :: ind   !< Number of the individual of the population
      class(class_ehist),                    intent(in)  :: ehist !< Evolution history
      real(8), dimension(:),                 intent(out) :: x     !< Trial individual
      integer, optional,                     intent(out) :: es    !< Exit status

      if (present(es)) es = 0

      call get_random_individual(ehist%nu, ehist%xmin, ehist%xmax, x)

   end subroutine

end module
