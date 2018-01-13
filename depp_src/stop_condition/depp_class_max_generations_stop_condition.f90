
!> \brief Defines a stop condition class which stops if the number of generations are greater
!! or equal to a specified value.

module mod_class_max_generation_stop_condition

   use mod_class_abstract_stop_condition
   use mod_class_ehist
   use mod_class_system_variables
   use mod_class_ifile
   use mod_string

   implicit none


   ! Makes everything private, except otherwise stated
   private

   !> \brief Class for creating a maximum number of generation based stop condition object
   type, public, extends(class_abstract_stop_condition) :: class_max_generation_stop_condition

      private
      integer :: g        !< Current generation
      integer :: ng       !< Maximum number of generations allowed
      logical :: stopflag !< Stop flag

   contains

      procedure, public, pass :: init                        !< Constructor
      procedure, public, pass :: compute_stop_condition      !< Computes stop condition
      procedure, public, pass :: is_stop_condition_satisfied !< Checks if stop condition is satisfied
      procedure, public, pass :: convergence_info            !< Returns a string containing convergence information

   end type

contains


   !> \brief Constructor
   subroutine init(this, sys_var)
      implicit none
      class(class_max_generation_stop_condition) :: this    !< A reference to this object
      class(class_system_variables),  intent(in) :: sys_var !< System's variables

      ! Inner variables
      type(class_ifile) :: ifile


      ! Reading configuration file
      call ifile%init(filename=trim(sys_var%absparfile), field_separator="&")
      call ifile%load()
      call ifile%get_value(this%ng,"ng")

      this%stopflag = .false.

   end subroutine


   !> \brief Computes the stop condition
   subroutine compute_stop_condition(this, ehist)
      implicit none
      class(class_max_generation_stop_condition) :: this  !< A reference to this object
      class(class_ehist),             intent(in) :: ehist !< Evolution history

      this%g = ehist%g

      if (this%g >= this%ng ) this%stopflag = .true.

   end subroutine


   !> \brief Checks if the stop condition was reached
   logical function is_stop_condition_satisfied(this)
      implicit none
      class(class_max_generation_stop_condition) :: this !< A reference to this object

      is_stop_condition_satisfied = this%stopflag

   end function

   !> \brief Returns the convergence information
   function convergence_info(this) result(str)
      implicit none
      class(class_max_generation_stop_condition) :: this !< A reference to this object
      character(len=:), allocatable              :: str  !< String containing convergence information

      ! Inner variables
      character(len=str_size) caux

      write(caux,"(A, I7, A, I7, A)") "   --->  Current generation: ", this%g, ". Tolerance: ", this%ng, "."

      str = trim(caux)

   end function

end module
