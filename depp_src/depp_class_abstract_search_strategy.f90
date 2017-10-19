
!> \brief Defines an abstract class for creating search strategy objects

module mod_class_abstract_search_strategy
   implicit none

   ! Makes everything private
   private

   ! Public class defining an interface for creating search strategy objects
   type, abstract, public :: class_abstract_search_strategy

   contains

      procedure(get_trial_interface), pass, deferred, public :: get_trial

   end type

   abstract interface

      subroutine get_trial_interface(this, ind, pop, x)
         import class_abstract_search_strategy
         implicit none
         class(class_abstract_search_strategy) :: this
         integer,                  intent(in)  :: ind !< number of the individual
         real(8), dimension(:,:),  intent(in)  :: pop !< population
         real(8), dimension(:),    intent(out) :: x

      end subroutine

   end interface

end module
