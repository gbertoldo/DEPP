!    DEPP - Differential Evolution Parallel Program
!
!    Copyright (C) 2019 by Jonas Joacir Radtke, Guilherme Bertoldo and Carlos Henrique Marchi
!
!    This program is free software: you can redistribute it and/or modify
!    it under the terms of the GNU General Public License as published by
!    the Free Software Foundation, either version 3 of the License, or
!    (at your option) any later version.
!
!    This program is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU General Public License for more details.
!
!    You should have received a copy of the GNU General Public License
!    along with this program.  If not, see <http://www.gnu.org/licenses/>.
!    
!    Contact:
!          Jonas Joacir Radtke (a)
!                 E-mail: jonas.radtke@gmail.com
!
!          Guilherme Bertoldo (a)
!                 E-mail: glbertoldo@gmail.com
!
!          Carlos Henrique Marchi (b)
!                 E-mail: chmcfd@gmail.com
!    Institution
!          (a) Federal University of Technology - Paraná - UTFPR
!              Linha Santa Bárbara, s/n, Francisco Beltrão, Paraná, Brazil
!              Zip Code 85601-970
!              
!          (b) Federal University of Paraná - UFPR
!              Curitiba, Paraná, Brazil
!              Caixa postal 19040
!              Zip Code 81531-980
!

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
      integer                            :: GNoAcc           !< Maximum number of generations allowed before stopping if no improvement was found
      real(8), allocatable, dimension(:) :: fbest            !< Register the last GNoAcc best values of the fitness function
      logical                            :: stopflag         !< Stop flag
      integer                            :: nwi              !< Number of generations without improvement
      integer                            :: ireg             !< Current register
      character(len=:), allocatable      :: backup_file      !< Backup file
      integer                            :: verbosity        !< Verbosity level for log
      integer                            :: save_backup_flag !< Save backup flag (0=no, 1=yes)


   contains

      procedure, public,  pass :: init                        !< Constructor
      procedure, public,  pass :: compute_stop_condition      !< Computes stop condition
      procedure, public,  pass :: is_stop_condition_satisfied !< Checks if stop condition is satisfied
      procedure, public,  pass :: convergence_info            !< Returns a string containing convergence information
      procedure, public,  pass :: final_convergence_info      !< Returns a string containing final convergence information
      procedure, private, pass :: add                         !< Adds data to the convergence history
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
      integer :: reload
      type(class_ifile) :: ifile


      ! Reading configuration file
      call ifile%init(filename=trim(sys_var%absparfile), field_separator="&")
      call ifile%load()
      call ifile%get_value(          this%GNoAcc,        "GNoAcc")
      call ifile%get_value(               reload,        "reload")
      call ifile%get_value(       this%verbosity,     "verbosity")
      call ifile%get_value(this%save_backup_flag,   "save_backup")

      ! Backup file
      this%backup_file = trim(sys_var%absfolderbkp) // "class_no_improvement_stop_condition_backup.txt"


      ! Allocating resources

      if ( allocated(this%fbest) ) deallocate(this%fbest)

      allocate(this%fbest(this%GNoAcc))

      this%fbest = 0.d0

      this%stopflag = .false.

      this%nwi = 0

      this%ireg = 0

      if (reload==1) call this%load_backup(sys_var)

   end subroutine


   !> \brief Computes the stop condition
   subroutine compute_stop_condition(this, ehist)
      implicit none
      class(class_no_improvement_stop_condition) :: this  !< A reference to this object
      class(class_ehist),             intent(in) :: ehist !< Evolution history

      ! Inner variables
      integer :: i
      integer :: k

      ! Creating labels
      associate ( ireg     => this%ireg,     &
                  GNoAcc   => this%GNoAcc,   &
                  stopflag => this%stopflag, &
                  nwi      => this%nwi,      &
                  fbest    => this%fbest     )


         ! Adding the max fitness of the current generation to the convergence history
         call this%add(maxval(ehist%fit))

         ! Checking convergence

         stopflag = .false.

         nwi = 0

         k = min(ireg, GNoAcc)

         do i = k, 1, -1

            ! Checks if there are differences among the best fitness in the last GNoAcc generations
            if ( fbest(k)-fbest(i) > 10.d0 * epsilon(1.d0) ) exit

            nwi = nwi + 1

         end do

         if ( nwi == GNoAcc ) stopflag = .true.

      end associate


      call this%save_backup()

   end subroutine


   !> \brief Adds data to the convergence history
   subroutine add(this, fmax)
      implicit none
      class(class_no_improvement_stop_condition) :: this  !< A reference to this object
      real(8),                        intent(in) :: fmax  !< Maximum value of f of current generation

      ! Creating labels
      associate ( ireg   => this%ireg,   &
                  GNoAcc => this%GNoAcc, &
                  fbest  => this%fbest   )

         ireg = ireg + 1

         if ( ireg <= GNoAcc ) then

            fbest(ireg) = fmax

         else

            call shift_left(fbest)

            fbest(GNoAcc) = fmax

         end if

      end associate

   contains

      !> \brief Shift data to left
      subroutine shift_left(x)
         implicit none
         real(8), dimension(:), intent(inout) :: x

         ! Inner variables
         integer :: i

         do i = 1, size(x)-1

            x(i) = x(i+1)

         end do

      end subroutine

   end subroutine


   !> \brief Checks if the best individual of the population
   !! stagnated in the last GNoAcc generations.
   logical function is_stop_condition_satisfied(this)
      implicit none
      class(class_no_improvement_stop_condition) :: this !< A reference to this object

      is_stop_condition_satisfied = this%stopflag

   end function


   !> \brief Returns the convergence information
   function convergence_info(this) result(str)
      implicit none
      class(class_no_improvement_stop_condition) :: this !< A reference to this object
      character(len=:), allocatable              :: str  !< String containing convergence information

      ! Inner variables
      character(len=str_size) caux

      caux = ""

      select case (this%verbosity)
          case (0)

          case (1)

             write(caux,"(A, I7, A, I7, A)") &
                "   --->  Number of generations without improving the best fitness: ", &
                this%nwi, ". Tolerance: ", this%GNoAcc, "."

          case (2)

             write(caux,"(A, I7, A, I7, A)") &
                "   --->  Number of generations without improving the best fitness: ", &
                this%nwi, ". Tolerance: ", this%GNoAcc, "."

          case default

      end select

      str = trim(caux)

   end function


   !> \brief Returns the final convergence information
   function final_convergence_info(this) result(str)
      implicit none
      class(class_no_improvement_stop_condition) :: this !< A reference to this object
      character(len=:), allocatable              :: str  !< String containing convergence information

      ! Inner variables
      character(len=str_size) caux

      caux = ""

      write(caux,"(A, I23, A)") trim(caux),    this%nwi, " : number of generations without improving best fitness"//char(10)
      write(caux,"(A, I23, A)") trim(caux), this%GNoAcc, " : maximum number of generations without improving best fitness"

      str = trim(caux)

   end function


   !> \brief Saves a backup of the class state
   subroutine save_backup(this)
      implicit none
      class(class_no_improvement_stop_condition) :: this !< A reference to this object

      if (mpio%master .and. this%save_backup_flag==1 ) then

         open(20, file=this%backup_file)

         write(20,*) this%ireg
         write(20,*) this%fbest
         write(20,*) this%stopflag
         write(20,*) this%nwi

         close(20)

      end if

   end subroutine


   !> \brief Loads a backup of the class state
   subroutine load_backup(this, sys_var)
      implicit none
      class(class_no_improvement_stop_condition) :: this    !< A reference to this object
      class(class_system_variables), intent(in)  :: sys_var !< System's variables

      ! Inner variables
      logical :: exists = .false.

      inquire( file = this%backup_file, exist = exists)

      if ( exists ) then

         open(20, file=this%backup_file)

         read(20,*) this%ireg
         read(20,*) this%fbest
         read(20,*) this%stopflag
         read(20,*) this%nwi

         close(20)

      else

         call sys_var%logger%println("No backup file found. Stopping...")

         ! Finishing MPI
         call mod_mpi_finalize()

      end if

   end subroutine

end module
