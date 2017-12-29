
!> \brief Provides an interface for a factory class for creating search strategy objects

module mod_class_abstract_search_strategy_factory

   use mod_class_system_variables
   use mod_class_abstract_search_strategy

   implicit none

   !> Interface for a factory class for creating search strategy objects
   type, abstract, public :: class_abstract_search_strategy_factory

   contains

      procedure(create_interface), deferred, public, pass :: create

   end type

   abstract interface

      !> Creates an instance of the search strategy object
      subroutine create_interface(this, sys_var, conf_file_name, searcher)

         import class_abstract_search_strategy_factory
         import class_system_variables
         import class_abstract_search_strategy

         implicit none
         class(class_abstract_search_strategy_factory)               :: this
         class(class_system_variables),                  intent(in)  :: sys_var
         character(len=*),                               intent(in)  :: conf_file_name
         class(class_abstract_search_strategy), pointer, intent(out) :: searcher


      end subroutine

   end interface

end module
