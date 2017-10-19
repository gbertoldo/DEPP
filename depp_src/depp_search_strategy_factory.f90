
!> \brief Provides a simple factory subroutine for creating individual generators

module mod_search_strategy_factory

   use mod_class_abstract_search_strategy
   use mod_class_DE_RAND_1

   implicit none

contains

   !> Creates an instance of the individual generator
   subroutine create_search_strategy(model, ind_gen)
      implicit none
      character(len=*), intent(in) :: model
      class(class_abstract_search_strategy), pointer, intent(out) :: ind_gen


      if ( .not. associated(ind_gen) ) then

         ! Reading configuration file


         ! Allocating the selected model

         if ( trim(model) == "DE/RAND/1" ) then

            allocate(class_DE_RAND_1::ind_gen)

         else

            write(*,*) "Unknown individual generator model. Stopping."

            stop

         end if


         ! Initializing the object
         select type (ind_gen)

            type is ( class_DE_RAND_1 )

               call ind_gen%init()

         end select

      else

         write(*,*) "Pointer already associated. Stopping."
         stop

      end if

   end subroutine

end module
