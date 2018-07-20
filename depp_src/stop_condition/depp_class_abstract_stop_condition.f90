
!> \brief Defines an abstract class for creating stop condition objects

module mod_class_abstract_stop_condition

   use mod_class_ehist

   implicit none

   ! Makes everything private, except otherwise stated
   private

   !> \brief Public class defining an interface for creating stop condition objects
   type, abstract, public :: class_abstract_stop_condition

   contains

      procedure(compute_stop_condition_interface),      pass, deferred, public :: compute_stop_condition      !< Computes stop condition
      procedure(is_stop_condition_satisfied_interface), pass, deferred, public :: is_stop_condition_satisfied !< Checks if stop condition is satisfied
      procedure(convergence_info_interface)           , pass, deferred, public :: convergence_info            !< Returns a string containing convergence information
      procedure(final_convergence_info_interface)     , pass, deferred, public :: final_convergence_info      !< Returns a string containing final convergence information

   end type

   abstract interface

      !> \brief Computes stop condition
      subroutine compute_stop_condition_interface(this, ehist)
         import class_abstract_stop_condition
         import class_ehist
         implicit none
         class(class_abstract_stop_condition)  :: this !< A reference to this object
         class(class_ehist),       intent(in)  :: ehist

      end subroutine

      !> \brief Checks if stop condition is satisfied
      logical function is_stop_condition_satisfied_interface(this)
         import class_abstract_stop_condition
         implicit none
         class(class_abstract_stop_condition)  :: this !< A reference to this object

      end function

      !> \brief Returns a string containing convergence information
      function convergence_info_interface(this) result(str)
         import class_abstract_stop_condition
         implicit none
         class(class_abstract_stop_condition) :: this !< A reference to this object
         character(len=:), allocatable        :: str

      end function

      !> \brief Returns a string containing final convergence information
      function final_convergence_info_interface(this) result(str)
         import class_abstract_stop_condition
         implicit none
         class(class_abstract_stop_condition) :: this !< A reference to this object
         character(len=:), allocatable        :: str

      end function

   end interface

end module
