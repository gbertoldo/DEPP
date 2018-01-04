
!> \brief Defines a factory for creation of RSM objects

module mod_class_RSM_factory

   use mod_class_abstract_RSM
   use mod_class_RSM_Quadratic_Model
   use mod_class_RSM_Incomplete_Quadratic_Model

   implicit none

   ! Makes everything private, except otherwise stated
   private

   !> \brief Defines a factory for creation of RSM objects
   type, public :: class_RSM_factory

   contains

      procedure, pass, public :: create

   end type

contains


   !> \brief Creates the RSM object
   subroutine create(this, option, obj)
      implicit none
      class(class_RSM_factory)                        :: this
      character(len=*),                   intent(in)  :: option
      class(class_abstract_RSM), pointer, intent(out) :: obj

      if ( trim(adjustl(option)) == "Quadratic" ) then

         allocate(class_RSM_Quadratic_Model::obj)

      else if ( trim(adjustl(option)) == "Incomplete_Quadratic" ) then

         allocate(class_RSM_Incomplete_Quadratic_Model::obj)

      else

         write(*,*) "class_RSM_factory: Unknown response surface model. Stopping..."

         stop

      end if

   end subroutine


end module

