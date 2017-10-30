
!> \brief Extends timer to save and read backup.

module mod_class_btimer

   use mod_class_system_variables
   use mod_class_ifile
   use mod_class_timer

   implicit none

   ! All members are private by default
   private

   ! Class declaration
   type, extends(class_timer), public :: class_btimer

      private

      real(8)                       :: backup_tcpu = 0.d0
      character(len=:), allocatable :: backup_file


   contains

      procedure, public, pass :: init
      procedure, public, pass :: save_backup
      procedure, public, pass :: load_backup

   end type

contains


   !> \brief Constructor
   subroutine init(this, sys_var)
      class(class_btimer)                       :: this
      class(class_system_variables), intent(in) :: sys_var

      ! Inner variables
      type(class_ifile) :: ifile     ! Input file reader
      integer           :: reload    ! Upload backup data


      ! Creating backup file name
      this%backup_file = trim(sys_var%absfolderout) // "/cpu_time_backup.txt"


      ! Checking reload option
      call ifile%init(filename=sys_var%absparfile, field_separator="&")
      call ifile%load()
      call ifile%get_value(reload, "reload")


      ! Initializing timer
      if ( reload == 0 ) then

         call this%start()

      else

         call this%load_backup()

         call this%start(this%backup_tcpu)

      end if


   end subroutine


   !> \brief Save backup
   subroutine save_backup(this)
      class(class_btimer) :: this

      open(1000,file=this%backup_file)

      write(1000,*) this%elapsed_time()

      close(1000)

   end subroutine


   !> \brief Load backup
   subroutine load_backup(this)
      class(class_btimer) :: this

      open(1000,file=this%backup_file)

      write(1000,*) this%backup_tcpu

      close(1000)

   end subroutine

end module
