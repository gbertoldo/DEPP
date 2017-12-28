
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
      procedure(feed_back_interface), pass, deferred, public :: feed_back
      procedure,                      pass,           public :: data_size
      procedure,                      pass,           public :: send
      procedure,                      pass,           public :: recv
      procedure,                      pass,           public :: update

   end type

   abstract interface


      subroutine get_trial_interface(this, ind, ehist, x)
         import class_abstract_search_strategy
         import class_ehist
         implicit none
         class(class_abstract_search_strategy) :: this
         integer,                  intent(in)  :: ind     ! Number of the individual of the population
         class(class_ehist),       intent(in)  :: ehist   ! Evolution history
         real(8), dimension(:),    intent(out) :: x       ! Trial individual

      end subroutine


      subroutine feed_back_interface(this, ind, ehist, fit, ecode)
         import class_abstract_search_strategy
         import class_ehist
         implicit none
         class(class_abstract_search_strategy) :: this
         integer,                  intent(in)  :: ind     ! Number of the individual of the population
         class(class_ehist),       intent(in)  :: ehist   ! Evolution history
         real(8),                  intent(in)  :: fit     ! Fitness of the trial individual
         integer,                  intent(in)  :: ecode   ! Error code

      end subroutine

   end interface


contains


      integer function data_size(this)
         implicit none
         class(class_abstract_search_strategy) :: this

         ! By default, no data is shared among threads
         data_size = 0

      end function


      subroutine send(this, i, to_thread)
         implicit none
         class(class_abstract_search_strategy) :: this
         integer,                   intent(in) :: i
         integer,                   intent(in) :: to_thread

      end subroutine


      subroutine recv(this, i, from_thread)
         implicit none
         class(class_abstract_search_strategy) :: this
         integer,                   intent(in) :: i
         integer,                   intent(in) :: from_thread

      end subroutine


      subroutine update(this)
         implicit none
         class(class_abstract_search_strategy) :: this

      end subroutine


end module
