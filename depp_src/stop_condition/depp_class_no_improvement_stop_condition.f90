
!> \brief Defines a stop condition class which stops if no improvement is found within a pre-defined number
!! of generations.

module mod_class_no_improvement_stop_condition

   use mod_class_abstract_stop_condition
   use mod_class_ehist
   use mod_class_system_variables
   use mod_class_ifile
   use mod_global_parameters

   implicit none


   ! Makes everything private, except otherwise stated
   private

   ! Public class for creating stop condition objects based on no improvement of the best fitness
   type, public, extends(class_abstract_stop_condition) :: class_no_improvement_stop_condition

      private
      integer                            :: GNoAcc   !< Maximum number of generations allowed before stopping if no improvement was found
      real(8), allocatable, dimension(:) :: fbest    !< Register the last GNoAcc best values of the fitness function
      logical                            :: stopflag !< Stop flag
      integer                            :: nwi      !< Number of generations without improvement

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
      class(class_no_improvement_stop_condition) :: this
      class(class_system_variables),  intent(in) :: sys_var

      ! Inner variables
      integer :: i
      type(class_ifile) :: ifile


      ! Reading configuration file
      call ifile%init(filename=trim(sys_var%absparfile), field_separator="&")
      call ifile%load()
      call ifile%get_value(this%GNoAcc,"GNoAcc")

      ! Allocating resources

      if ( allocated(this%fbest) ) deallocate(this%fbest)

      allocate(this%fbest(this%GNoAcc))


      do i = 1, this%GNoAcc

         this%fbest(i) = -huge(1.d0)/dble(i)

      end do

      this%stopflag = .false.

      this%nwi = 0

   end subroutine


   !> \brief Computes the stop condition
   subroutine compute_stop_condition(this, ehist)
      implicit none
      class(class_no_improvement_stop_condition) :: this
      class(class_ehist),             intent(in) :: ehist

      ! Inner variables
      integer       :: i
      integer, save :: ireg = 0

      ! Getting the index of the current register
      ireg = idx(ireg+1,this%GNoAcc)

      this%fbest(ireg) = maxval(ehist%fit)


      this%stopflag = .true.

      this%nwi = this%GNoAcc

      do i = 1, this%GNoAcc

         ! Checks if there are differences among the best fitness in the last GNoAcc generations
         if ( abs(this%fbest(i)-this%fbest(this%GNoAcc)) > 10.d0 * epsilon(1.d0) ) then

            this%stopflag = .false.

            this%nwi = i-1

            return

         end if

      end do

   end subroutine


   !> \brief Checks if the best individual of the population
   !! stagnated in the last GNoAcc generations.
   logical function is_stop_condition_satisfied(this)
      implicit none
      class(class_no_improvement_stop_condition) :: this

      is_stop_condition_satisfied = this%stopflag

   end function


   !> \brief Returns the index of the current register
   integer function idx(i, nps)
      implicit none
      integer, intent(in) :: i
      integer, intent(in) :: nps

      idx = mod(i,nps)

      if (idx==0) idx = nps

   end function


   !> Returns the convergence information
   function convergence_info(this) result(str)
      implicit none
      class(class_no_improvement_stop_condition) :: this
      character(len=:), allocatable              :: str

      ! Inner variables
      character(len=str_size) caux

      write(caux,"(A, I7, A, I7, A)") &
         "   --->  Number of generations without improving best fitness: ", &
         this%nwi, ". Tolerance: ", this%GNoAcc, "."

      str = trim(caux)

   end function

end module
