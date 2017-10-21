
!> \brief Defines a composite stop condition class.

module mod_class_composite_stop_condition

   use mod_class_abstract_stop_condition
   use mod_stop_condition_factory
   use mod_class_ehist
   use mod_class_system_variables
   use mod_class_ifile
   use mod_global_parameters

   implicit none


   ! Makes everything private, except otherwise stated
   private


   ! A pointer to a stop condition object
   type :: ptr_stop_cond

      class(class_abstract_stop_condition), pointer :: ptr => null()

   end type


   ! Public class for creating a composite of stop condition objects
   type, public, extends(class_abstract_stop_condition) :: class_composite_stop_condition

      private
      integer :: nsc      !< Number of stop conditions
      logical :: stopflag !< Stop flag

      type(ptr_stop_cond), allocatable, dimension(:) :: stop_cond_container

   contains

      procedure, public, pass :: init
      procedure, public, pass :: compute_stop_condition
      procedure, public, pass :: is_stop_condition_satisfied
      procedure, public, pass :: convergence_info

   end type

contains


   !> \brief Constructor
   subroutine init(this, sys_var)
      implicit none
      class(class_composite_stop_condition)      :: this
      class(class_system_variables),  intent(in) :: sys_var

      ! Inner variables
      type(class_ifile)  :: ifile
      ! Auxiliary variables
      integer :: i
      integer :: IO
      character(len=str_size) :: caux
      character(len=str_size) :: models(100)


      ! Reading configuration file
      call ifile%init(filename=trim(sys_var%absparfile), field_separator="&")
      call ifile%load()
      call ifile%get_value(caux,"composite_stop_condition")

      ! Identifying stop conditions
      models = "null"

      read(caux,*, IOstat=IO) models

      do i = 1, size(models)

         if (trim(models(i))=="null") then

            this%nsc = i-1

            exit

         end if

      end do

      ! Creating stop condition objects

      allocate(this%stop_cond_container(this%nsc))

      do i = 1, this%nsc

         call create_stop_condition_object(sys_var, model=models(i), stopper=this%stop_cond_container(i)%ptr)

      end do

      this%stopflag = .false.

   end subroutine


   !> \brief Computes the stop condition
   subroutine compute_stop_condition(this, ehist)
      implicit none
      class(class_composite_stop_condition)      :: this
      class(class_ehist),             intent(in) :: ehist

      ! Inner variables
      integer :: i

      do i = 1, this%nsc

         call this%stop_cond_container(i)%ptr%compute_stop_condition(ehist)

      end do

   end subroutine


   !> \brief Checks if the stop condition is satisfied
   logical function is_stop_condition_satisfied(this)
      implicit none
      class(class_composite_stop_condition) :: this

      ! Inner variables
      integer :: i

      is_stop_condition_satisfied = .false.

      do i = 1, this%nsc

         if ( this%stop_cond_container(i)%ptr%is_stop_condition_satisfied() ) then

            is_stop_condition_satisfied = .true.

         end if

      end do

   end function


   !> Returns the convergence information
   function convergence_info(this) result(str)
      implicit none
      class(class_composite_stop_condition) :: this
      character(len=:), allocatable         :: str

      ! Inner variables
      integer :: i

      str = "Convergence info: " // char(10) ! char(10) = new line char

      do i = 1, this%nsc

         str = str // this%stop_cond_container(i)%ptr%convergence_info() // char(10)

      end do

   end function

end module
