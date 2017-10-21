
!> \brief Defines an abstract class for creating stop condition objects

module mod_class_abstract_stop_condition

   use mod_class_ehist

   implicit none

   ! Makes everything private, except otherwise stated
   private

   ! Public class defining an interface for creating stop condition objects
   type, abstract, public :: class_abstract_stop_condition

   contains

      procedure(compute_stop_condition_interface),      pass, deferred, public :: compute_stop_condition
      procedure(is_stop_condition_satisfied_interface), pass, deferred, public :: is_stop_condition_satisfied
      procedure(convergence_info_interface)           , pass, deferred, public :: convergence_info

   end type

   abstract interface

      subroutine compute_stop_condition_interface(this, ehist)
         import class_abstract_stop_condition
         import class_ehist
         implicit none
         class(class_abstract_stop_condition)  :: this
         class(class_ehist),       intent(in)  :: ehist

      end subroutine

      logical function is_stop_condition_satisfied_interface(this)
         import class_abstract_stop_condition
         implicit none
         class(class_abstract_stop_condition)  :: this

      end function

      function convergence_info_interface(this) result(str)
         import class_abstract_stop_condition
         implicit none
         class(class_abstract_stop_condition) :: this
         character(len=:), allocatable        :: str

      end function

   end interface

end module
