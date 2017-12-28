
!> \brief Provides a simple factory subroutine for creating search strategy objects

module mod_search_strategy_factory

   use mod_class_system_variables
   use mod_class_abstract_search_strategy
   use mod_class_DE_RAND_1
   use mod_class_DE_RSM
   use mod_class_ifile

   implicit none

contains

   !> Creates an instance of the individual generator
   recursive subroutine create_search_strategy(sys_var, kh, kss, searcher)
      implicit none
      class(class_system_variables),                  intent(in)  :: sys_var
      INTEGER,                               intent(in)  :: kh
      INTEGER,                               intent(in)  :: kss
      class(class_abstract_search_strategy), pointer, intent(out) :: searcher


      ! Inner variables
      class(class_abstract_search_strategy), pointer :: aux => null()
      type(class_ifile) :: ifile
      integer :: np



      if ( .not. associated(searcher) ) then

         ! Reading configuration file

         ! Allocating the selected model

         if ( kh == 2 ) then

            allocate(class_DE_RSM::searcher)

         else if ( kh == 1 ) then

         else if ( kh == 0 ) then

            if ( kss == 1 ) then

               allocate(class_DE_RAND_1::searcher)

            end if

         else

            write(*,*) "Unknown hybridization model. Stopping."

            stop

         end if


         ! Initializing the object
         select type (searcher)

            type is ( class_DE_RAND_1 )

               call searcher%init()

            type is ( class_DE_RSM )

               call create_search_strategy(sys_var, 0, kss, aux)

               call ifile%init(filename=sys_var%absparfile, field_separator='&')

               call ifile%load()

               call ifile%get_value(np,"np")

               call searcher%init(sys_var, np, aux)

         end select

      else

         write(*,*) "Pointer already associated. Stopping."
         stop

      end if

   end subroutine

end module
