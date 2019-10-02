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

!> \brief Extends timer to save and read backup.

module mod_class_btimer

   use mod_mpi
   use mod_class_system_variables
   use mod_class_ifile
   use mod_class_timer

   implicit none

   ! All members are private by default
   private

   ! Class declaration
   type, extends(class_timer), public :: class_btimer

      private

      real(8)                       :: backup_tcpu = 0.d0 !< Elapsed time
      character(len=:), allocatable :: backup_file        !< Backup file
      integer                       :: save_backup_flag   !< Save backup flag (0=no, 1=yes)


   contains

      procedure, public, pass :: init        !< Constructor
      procedure, public, pass :: save_backup !< Saves backup
      procedure, public, pass :: load_backup !< Loads backup

   end type

contains


   !> \brief Constructor
   subroutine init(this, sys_var)
      class(class_btimer)                       :: this    !< A reference to this object
      class(class_system_variables), intent(in) :: sys_var !< System's variables

      ! Inner variables
      type(class_ifile) :: ifile     ! Input file reader
      integer           :: reload    ! Upload backup data


      ! Creating backup file name
      this%backup_file = trim(sys_var%absfolderbkp) // "/cpu_time_backup.txt"


      ! Checking reload option
      call ifile%init(filename=sys_var%absparfile, field_separator="&")
      call ifile%load()
      call ifile%get_value(this%save_backup_flag,   "save_backup")
      call ifile%get_value(               reload,        "reload")


      ! Initializing timer
      if ( reload == 0 ) then

         call this%start()

      else

         call this%load_backup(sys_var)

         call this%start(this%backup_tcpu)

      end if


   end subroutine


   !> \brief Save backup
   subroutine save_backup(this)
      class(class_btimer) :: this !< A reference to this object

      if ( mpio%master .and. this%save_backup_flag==1 ) then

         open(1000,file=this%backup_file)

         write(1000,*) this%elapsed_time()

         close(1000)

      end if

   end subroutine


   !> \brief Load backup
   subroutine load_backup(this, sys_var)
      class(class_btimer)                        :: this    !< A reference to this object
      class(class_system_variables), intent(in)  :: sys_var !< System's variables

      ! Inner variables
      logical :: exists = .false.

      inquire( file = this%backup_file, exist = exists)

      if ( exists ) then

         open(1000,file=this%backup_file)

         read(1000,*) this%backup_tcpu

         close(1000)

      else

         call sys_var%logger%println("No backup file found. Stopping...")

         ! Finishing MPI
         call mod_mpi_finalize()

      end if

   end subroutine

end module
