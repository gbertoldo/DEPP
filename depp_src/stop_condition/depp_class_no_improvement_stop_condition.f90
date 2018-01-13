
!> \brief Defines a stop condition class which stops if no improvement is found within a pre-defined number
!! of generations.

module mod_class_no_improvement_stop_condition

   use mod_class_abstract_stop_condition
   use mod_class_ehist
   use mod_class_system_variables
   use mod_class_ifile
   use mod_string
   use mod_mpi

   implicit none


   ! Makes everything private, except otherwise stated
   private

   !> \brief Class for creating stop condition objects based on no improvement of the best fitness
   type, public, extends(class_abstract_stop_condition) :: class_no_improvement_stop_condition

      private
      integer                            :: GNoAcc      !< Maximum number of generations allowed before stopping if no improvement was found
      real(8), allocatable, dimension(:) :: fbest       !< Register the last GNoAcc best values of the fitness function
      logical                            :: stopflag    !< Stop flag
      integer                            :: nwi         !< Number of generations without improvement
      integer                            :: ireg        !< Current register
      character(len=:), allocatable      :: backup_file !< Backup file

   contains

      procedure, public,  pass :: init                        !< Constructor
      procedure, public,  pass :: compute_stop_condition      !< Computes stop condition
      procedure, public,  pass :: is_stop_condition_satisfied !< Checks if stop condition is satisfied
      procedure, public,  pass :: convergence_info            !< Returns a string containing convergence information
      procedure, private, pass :: save_backup                 !< Saves a backup of the class state
      procedure, private, pass :: load_backup                 !< Loads a backup of the class state


   end type

contains


   !> \brief Constructor
   subroutine init(this, sys_var)
      implicit none
      class(class_no_improvement_stop_condition) :: this    !< A reference to this object
      class(class_system_variables),  intent(in) :: sys_var !< System's variables

      ! Inner variables
      integer :: i
      integer :: reload
      type(class_ifile) :: ifile


      ! Reading configuration file
      call ifile%init(filename=trim(sys_var%absparfile), field_separator="&")
      call ifile%load()
      call ifile%get_value(this%GNoAcc,"GNoAcc")
      call ifile%get_value(reload,"reload")

      ! Backup file
      this%backup_file = trim(sys_var%absfolderout) // "class_no_improvement_stop_condition_backup.txt"


      ! Allocating resources

      if ( allocated(this%fbest) ) deallocate(this%fbest)

      allocate(this%fbest(this%GNoAcc))


      do i = 1, this%GNoAcc

         this%fbest(i) = -huge(1.d0)/dble(i)

      end do

      this%stopflag = .false.

      this%nwi = 0

      this%ireg = 0

      if (reload==1) call this%load_backup()

   end subroutine


   !> \brief Computes the stop condition
   subroutine compute_stop_condition(this, ehist)
      implicit none
      class(class_no_improvement_stop_condition) :: this  !< A reference to this object
      class(class_ehist),             intent(in) :: ehist !< Evolution history

      ! Inner variables
      integer       :: i

      ! Getting the index of the current register
      this%ireg = idx(this%ireg+1,this%GNoAcc)

      this%fbest(this%ireg) = maxval(ehist%fit)


      this%stopflag = .true.

      this%nwi = this%GNoAcc

      do i = 1, this%GNoAcc

         ! Checks if there are differences among the best fitness in the last GNoAcc generations
         if ( abs(this%fbest(i)-this%fbest(this%GNoAcc)) > 10.d0 * epsilon(1.d0) ) then

            this%stopflag = .false.

            this%nwi = i-1

            exit

         end if

      end do

      call this%save_backup()

   end subroutine


   !> \brief Checks if the best individual of the population
   !! stagnated in the last GNoAcc generations.
   logical function is_stop_condition_satisfied(this)
      implicit none
      class(class_no_improvement_stop_condition) :: this !< A reference to this object

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


   !> \brief Returns the convergence information
   function convergence_info(this) result(str)
      implicit none
      class(class_no_improvement_stop_condition) :: this !< A reference to this object
      character(len=:), allocatable              :: str  !< String containing convergence information

      ! Inner variables
      character(len=str_size) caux

      write(caux,"(A, I7, A, I7, A)") &
         "   --->  Number of generations without improving best fitness: ", &
         this%nwi, ". Tolerance: ", this%GNoAcc, "."

      str = trim(caux)

   end function

   !> \brief Saves a backup of the class state
   subroutine save_backup(this)
      implicit none
      class(class_no_improvement_stop_condition) :: this !< A reference to this object

      if (mpio%master) then

         open(20, file=this%backup_file)

         write(20,*) this%ireg
         write(20,*) this%fbest
         write(20,*) this%stopflag
         write(20,*) this%nwi

         close(20)

      end if

   end subroutine


   !> \brief Loads a backup of the class state
   subroutine load_backup(this)
      implicit none
      class(class_no_improvement_stop_condition) :: this !< A reference to this object

      open(20, file=this%backup_file)

      read(20,*) this%ireg
      read(20,*) this%fbest
      read(20,*) this%stopflag
      read(20,*) this%nwi

      close(20)

   end subroutine

end module
