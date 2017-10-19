
! \brief Contains information about system variables

module mod_class_system_variables

   ! Input file reader
   use mod_class_ifile

   ! Global parameters module
   use mod_global_parameters


   implicit none

  ! Makes everything private, except otherwise stated
   private

   ! Type system variables contains information about paths and filenames
   type, public :: class_system_variables

      character(str_size) :: folderin  = "./depp_input/"  !< folder the for input files
      character(str_size) :: folderout = "./depp_output/" !< folder the for output files
      character(str_size) :: ifilename = "input_file.txt" !< stores the name of the parameters input file
      character(str_size) :: absfolderin                  !< absolute path to folderin
      character(str_size) :: absfolderout                 !< absolute path to folderout
      character(str_size) :: parfile                      !< name of the parameters input file
      character(str_size) :: absparfile                   !< absolute path and name of the parameters input file
      character(str_size) :: logfile                      !< name of the log file
      character(str_size) :: fdir                         !< name of working directory for fitness calculation
      character(str_size) :: ffit                         !< name of executable for fitness calculation
      character(str_size) :: CWD                          !< current working directory
   contains

      procedure, public, pass :: init

   end type

contains

   !> \brief Read simulation variables from input file
   subroutine init(this)
      implicit none
      class(class_system_variables) :: this

      ! Inner variables
      type(class_ifile)       :: ifile
      character(len=str_size) :: sname



      ! Getting the current working directory
      call get_environment_variable('PWD',this%CWD)



      ! Absolute path to input folder
      this%absfolderin  = trim(this%CWD) // "/" // trim(this%folderin)


      ! Reading the name of the input file
      call ifile%init(filename=trim(this%absfolderin)//trim(this%ifilename), field_separator='&')

      call ifile%load()

      call ifile%get_value(this%parfile,"parfile")

      this%absparfile = trim(this%absfolderin) // "/" // trim(this%parfile)



      ! Reading the input file

      call ifile%init(filename=trim(this%absfolderin)//trim(this%parfile), field_separator='&')

      call ifile%load()

      call ifile%get_value(this%fdir, "fdir")
      call ifile%get_value(this%ffit, "ffit")
      call ifile%get_value(sname,     "sname")


      ! folderout for current simulation
      this%folderout = trim(this%folderout) // trim(sname) // "/"


      ! Absolute path to input folder
      this%absfolderout = trim(this%CWD) // "/" // trim(this%folderout)


   end subroutine



end module
