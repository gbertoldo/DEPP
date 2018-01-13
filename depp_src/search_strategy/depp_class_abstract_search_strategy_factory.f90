
!> \brief Provides an interface for a factory class for creating search strategy objects

module mod_class_abstract_search_strategy_factory

   use mod_class_system_variables
   use mod_class_abstract_search_strategy

   implicit none

   !> \brief Interface for a factory class for creating search strategy objects
   type, abstract, public :: class_abstract_search_strategy_factory

   contains

      procedure(create_interface), deferred, public, pass :: create !< Creates a search strategy object

   end type

   abstract interface

      !> \brief Creates an instance of the search strategy object
      subroutine create_interface(this, sys_var, conf_file_name, searcher)

         import class_abstract_search_strategy_factory
         import class_system_variables
         import class_abstract_search_strategy

         implicit none
         class(class_abstract_search_strategy_factory)               :: this           !< A reference to this object
         class(class_system_variables),                  intent(in)  :: sys_var        !< System's variables
         character(len=*),                               intent(in)  :: conf_file_name !< Configuration file
         class(class_abstract_search_strategy), pointer, intent(out) :: searcher       !< Search strategy object


      end subroutine

   end interface

end module
