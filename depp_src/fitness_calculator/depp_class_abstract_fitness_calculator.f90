
!> \brief Defines an interface for fitness function calculation

module mod_class_abstract_fitness_calculator

   use mod_class_ehist

   implicit none


   ! Makes everything private, except otherwise stated
   private


   !> \brief Defines an interface for fitness function calculation
   type, abstract, public :: class_abstract_fitness_calculator

   contains

      procedure(get_fitness_interface),     deferred, public,  pass :: get_fitness
      procedure(statistics_info_interface), deferred, public,  pass :: statistics_info
      procedure(save_failure_interface),    deferred, private, pass :: save_failure
      procedure,                                      public,  pass :: data_size
      procedure,                                      public,  pass :: send
      procedure,                                      public,  pass :: recv
      procedure,                                      public,  pass :: update

   end type


   abstract interface


      subroutine get_fitness_interface(this, ehist, i, x, fit, ecode)
         import class_abstract_fitness_calculator
         import class_ehist
         implicit none
         class(class_abstract_fitness_calculator) :: this
         class(class_ehist),          intent(in)  :: ehist
         integer,                     intent(in)  :: i
         real(8), dimension(:),       intent(in)  :: x
         real(8),                     intent(out) :: fit
         integer,                     intent(out) :: ecode

      end subroutine


      function statistics_info_interface(this) result(info)
         import class_abstract_fitness_calculator
         implicit none
         class(class_abstract_fitness_calculator) :: this
         character(len=:), allocatable            :: info

      end function


      subroutine save_failure_interface(this, ehist, i, x, fit, ecode)
         import class_abstract_fitness_calculator
         import class_ehist
         implicit none
         class(class_abstract_fitness_calculator) :: this
         class(class_ehist),           intent(in) :: ehist
         integer,                      intent(in) :: i
         real(8), dimension(:),        intent(in) :: x
         real(8),                      intent(in) :: fit
         integer,                      intent(in) :: ecode

      end subroutine


   end interface


contains


      integer function data_size(this)
         implicit none
         class(class_abstract_fitness_calculator) :: this

         ! By default, no data is shared among threads
         data_size = 0

      end function


      subroutine send(this, i, to_thread)
         implicit none
         class(class_abstract_fitness_calculator) :: this
         integer,                      intent(in) :: i
         integer,                      intent(in) :: to_thread

      end subroutine


      subroutine recv(this, i, from_thread)
         implicit none
         class(class_abstract_fitness_calculator) :: this
         integer,                      intent(in) :: i
         integer,                      intent(in) :: from_thread

      end subroutine


      subroutine update(this)
         implicit none
         class(class_abstract_fitness_calculator) :: this

      end subroutine


end module
