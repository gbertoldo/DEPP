
!> \brief Defines an abstract class for creating search strategy objects

module mod_class_abstract_search_strategy

   use mod_class_ehist

   implicit none

   ! Makes everything private, except otherwise stated
   private

   ! Public class defining an interface for creating search strategy objects
   type, abstract, public :: class_abstract_search_strategy

   contains

      procedure(get_trial_interface), pass, deferred, public :: get_trial

   end type

   abstract interface

      subroutine get_trial_interface(this, ind, ehist, x, estatus)
         import class_abstract_search_strategy
         import class_ehist
         implicit none
         class(class_abstract_search_strategy) :: this
         integer,                  intent(in)  :: ind   ! Number of the individual of the population
         class(class_ehist),       intent(in)  :: ehist ! Evolution history
         real(8), dimension(:),    intent(out) :: x     ! Trial individual
         integer,                  intent(out) :: estatus ! to be removed

      end subroutine

   end interface

end module
