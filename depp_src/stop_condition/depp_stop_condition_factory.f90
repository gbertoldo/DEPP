
!> \brief Provides a simple factory subroutine for creating stop condition objects

module mod_stop_condition_factory

   use mod_class_system_variables
   use mod_class_abstract_stop_condition
   use mod_class_max_generation_stop_condition
   use mod_class_p_measure_stop_condition
   use mod_class_no_improvement_stop_condition
   use mod_mpi

   implicit none

contains

   !> \brief Creates an instance of the stop condition object
   subroutine create_stop_condition_object(sys_var, model, stopper)
      implicit none
      class(class_system_variables),                      intent(in) :: sys_var !< System's variables
      character(len=*),                                   intent(in) :: model   !< Model
      class(class_abstract_stop_condition),     pointer, intent(out) :: stopper !< Stop condition object


      if ( .not. associated(stopper) ) then

         ! Reading configuration file


         ! Allocating the selected model

         if ( trim(model) == "max_generations" ) then

            allocate(class_max_generation_stop_condition::stopper)

         else if ( trim(model) == "no_improvement" ) then

            allocate(class_no_improvement_stop_condition::stopper)

         else if ( trim(model) == "p_measure" ) then

            allocate(class_p_measure_stop_condition::stopper)

         else

            call sys_var%logger%print("Unknown stop condition model: " // trim(model) // ". Stopping.")

            call mod_mpi_finalize()

         end if


         ! Initializing the object
         select type (stopper)

            type is ( class_max_generation_stop_condition )

               call stopper%init(sys_var)

            type is ( class_no_improvement_stop_condition )

               call stopper%init(sys_var)

            type is ( class_p_measure_stop_condition )

               call stopper%init(sys_var)

         end select

      else

         call sys_var%logger%print("Variable already associated. Stopping.")

         call mod_mpi_finalize()

      end if

   end subroutine

end module
