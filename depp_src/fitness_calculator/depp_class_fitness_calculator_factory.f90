
!> \brief Defines a factory class for creating fitness calculator objects.

module mod_class_fitness_calculator_factory

   use mod_mpi
   use mod_class_system_variables
   use mod_class_ehist
   use mod_class_abstract_fitness_calculator
   use mod_class_external_fitness_calculator

   implicit none


   ! Makes everything private, except otherwise stated
   private


   !> \brief Factory class for creating fitness calculator objects
   type, public :: class_fitness_calculator_factory

   contains

      procedure, public, pass :: create !< Creates a fitness calculator object

   end type


contains


   !> \brief Creates a fitness calculator object
   subroutine create(this, sys_var, ehist, model, fitness_calculator)
      implicit none
      class(class_fitness_calculator_factory)                        :: this               !< A reference to this object
      class(class_system_variables),                     intent(in)  :: sys_var            !< System's variables
      class(class_ehist),                                intent(in)  :: ehist              !< Evolution history
      character(len=*),                                  intent(in)  :: model              !< Model
      class(class_abstract_fitness_calculator), pointer, intent(out) :: fitness_calculator !< Fitness calculator


      ! Allocating object

      if ( .not. associated(fitness_calculator) ) then

         if ( trim(model) == "EXTERNAL_CALCULATOR" ) then

            allocate(class_external_fitness_calculator::fitness_calculator)

         else

            call sys_var%logger%println("Unkown model. Stopping.")

            call mod_mpi_finalize()

         end if


         ! Initializing the object
         select type (fitness_calculator)

            type is ( class_external_fitness_calculator )

               call fitness_calculator%init(sys_var, ehist)

         end select

      else

         call sys_var%logger%println("Pointer already associated. Stopping.")

         call mod_mpi_finalize()

      end if


   end subroutine


end module
