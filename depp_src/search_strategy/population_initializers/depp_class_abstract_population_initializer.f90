
!> \brief Provides an interface for population initialization

module mod_class_abstract_population_initializer

   use mod_class_ehist

   implicit none

   ! Makes everything private, except otherwise stated
   private

   !> \brief Abstract class for population initialization
   type, abstract, public :: class_abstract_population_initializer

   contains

      procedure(get_trial_interface), deferred, pass, public :: get_trial !< Generates a trial individual

   end type

   abstract interface

      !> \brief Gets a trial individual
      subroutine get_trial_interface(this, ind, ehist, x, es)
         import class_abstract_population_initializer
         import class_ehist
         implicit none
         class(class_abstract_population_initializer) :: this  !< A reference to this object
         integer,                         intent(in)  :: ind   !< Number of the individual of the population
         class(class_ehist),              intent(in)  :: ehist !< Evolution history
         real(8), dimension(:),           intent(out) :: x     !< Trial individual
         integer, optional,               intent(out) :: es    !< Exit status

      end subroutine

   end interface

end module
