
!> \brief Defines an exit code for the fitness calculator

module mod_class_fitness_calculator_exit_code

   implicit none


   ! Makes everything private, except otherwise stated
   private


   !> \brief Class defining the exit code
   type, private :: class_fitness_calculator_exit_code

      integer :: SUCCESS
      integer :: FAIL_AND_GIVE_UP
      integer :: FAIL_AND_TRY_AGAIN

   end type


   ! Public object with the fitness exit code
   type(class_fitness_calculator_exit_code), parameter, public :: fitness_calculator_exit_code &

      = class_fitness_calculator_exit_code(0,1,2)


end module
