
!> \brief Defines an abstract class for creating search strategy objects

module mod_class_abstract_search_strategy

   use mod_class_ehist

   implicit none

   ! Makes everything private, except otherwise stated
   private

   !> \brief Public class defining an interface for creating search strategy objects
   type, abstract, public :: class_abstract_search_strategy

   contains

      procedure(get_trial_interface), pass, deferred, public :: get_trial !< Gets a trial individual
      procedure(feed_back_interface), pass, deferred, public :: feed_back !< Process the feedback from fitness calculator
      procedure,                      pass,           public :: data_size !< Gives the size of the shared data vector
      procedure,                      pass,           public :: send      !< Send data to other threads
      procedure,                      pass,           public :: recv      !< Receive data from other threads
      procedure,                      pass,           public :: update    !< Perform update calculations after parallel computation cycle

   end type

   abstract interface

      !> \brief Gets a trial individual
      subroutine get_trial_interface(this, ind, ehist, x, es)
         import class_abstract_search_strategy
         import class_ehist
         implicit none
         class(class_abstract_search_strategy) :: this  !< A reference to this object
         integer,                  intent(in)  :: ind   !< Number of the individual of the population
         class(class_ehist),       intent(in)  :: ehist !< Evolution history
         real(8), dimension(:),    intent(out) :: x     !< Trial individual
         integer, optional,        intent(out) :: es    !< Exit status

      end subroutine

      !> \brief Process the feedback from fitness calculator
      subroutine feed_back_interface(this, ind, ehist, fit, ecode)
         import class_abstract_search_strategy
         import class_ehist
         implicit none
         class(class_abstract_search_strategy) :: this    !< A reference to this object
         integer,                  intent(in)  :: ind     !< Number of the individual of the population
         class(class_ehist),       intent(in)  :: ehist   !< Evolution history
         real(8),                  intent(in)  :: fit     !< Fitness of the trial individual
         integer,                  intent(in)  :: ecode   !< Error code

      end subroutine

   end interface


contains

      !> \brief Gives the size of the shared data vector
      integer function data_size(this)
         implicit none
         class(class_abstract_search_strategy) :: this !< A reference to this object

         ! By default, no data is shared among threads
         data_size = 0

      end function

      !> \brief Send data to other threads
      subroutine send(this, i, to_thread)
         implicit none
         class(class_abstract_search_strategy) :: this      !< A reference to this object
         integer,                   intent(in) :: i         !< Index of the shared data
         integer,                   intent(in) :: to_thread !< Receiver thread

      end subroutine

      !> \brief Receive data from other threads
      subroutine recv(this, i, from_thread)
         implicit none
         class(class_abstract_search_strategy) :: this        !< A reference to this object
         integer,                   intent(in) :: i           !< Index of the shared data
         integer,                   intent(in) :: from_thread !< Sender thread

      end subroutine

      !> \brief Perform update calculations after parallel computation cycle
      subroutine update(this)
         implicit none
         class(class_abstract_search_strategy) :: this !< A reference to this object

      end subroutine


end module
