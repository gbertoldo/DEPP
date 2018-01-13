
!> \brief Contains information about system variables

module mod_class_system_variables

   use mod_mpi
   use mod_class_ifile
   use mod_string
   use mod_class_log_output_control
   use mod_class_calendar

   implicit none

  ! Makes everything private, except otherwise stated
   private

   !> \brief Class system variables contains information about paths and filenames
   !! as well as a logger to print output data.
   type, public :: class_system_variables

      character(str_size) :: sname                        !< Simulation id
      character(str_size) :: folderin  = "./depp_input/"  !< Folder the for input files
      character(str_size) :: folderout = "./depp_output/" !< Folder the for output files
      character(str_size) :: ifilename = "input_file.txt" !< Stores the name of the parameters input file
      character(str_size) :: absfolderin                  !< Absolute path to folderin
      character(str_size) :: absfolderout                 !< Absolute path to folderout
      character(str_size) :: parfile                      !< Name of the parameters input file
      character(str_size) :: absparfile                   !< Absolute path and name of the parameters input file
      character(str_size) :: logfile                      !< Name of the log file
      character(str_size) :: abslogfile                   !< Absolute path to log file
      character(str_size) :: fdir                         !< Name of working directory for fitness calculation
      character(str_size) :: ffit                         !< Name of executable for fitness calculation
      character(str_size) :: CWD                          !< Current working directory

      ! Defines an instance of class_log_output_control
      type(class_log_output_control) :: logger            !< Logger for output data
      type(class_calendar)           :: calendar          !< Instance of calendar class


   contains

      procedure, public, pass :: init                     !< Constructor

   end type

contains

   !> \brief Reads simulation variables from input file
   subroutine init(this)
      implicit none
      class(class_system_variables) :: this !< A reference to this object

      ! Inner variables
      type(class_ifile)       :: ifile
      integer                 :: reload
      logical                 :: lexist


      ! Getting the current working directory
      call get_environment_variable('PWD',this%CWD)


      ! Absolute path to input folder
      this%absfolderin  = trim(this%CWD) // "/" // trim(this%folderin)


      ! Reading the name of the input file
      call ifile%init(filename=trim(this%absfolderin)//trim(this%ifilename), field_separator='&')
      call ifile%load()
      call ifile%get_value(this%parfile,"parfile")


      ! Absolute path to parameter file
      this%absparfile = trim(this%absfolderin) // "/" // trim(this%parfile)


      ! Reading the input file

      call ifile%init(filename=trim(this%absfolderin)//trim(this%parfile), field_separator='&')

      call ifile%load()

      call ifile%get_value(this%fdir,    "fdir")
      call ifile%get_value(this%ffit,    "ffit")
      call ifile%get_value(this%sname,  "sname")
      call ifile%get_value(reload,     "reload")


      ! folderout for current simulation
      this%folderout = trim(this%folderout) // trim(this%sname) // "/"


      ! Absolute path to input folder
      this%absfolderout = trim(this%CWD) // "/" // trim(this%folderout)


      ! Only for master
      if (mpio%master) then

         ! Creating output folder
         inquire(file = trim(this%absfolderout), exist = lexist)

         if (lexist) then

            if (reload == 0) then
               call execute_command_line("rm -r " // trim(this%absfolderout))
               call execute_command_line("mkdir " // trim(this%absfolderout))
            end if

         else

            call execute_command_line("mkdir " // trim(this%absfolderout))

         end if

      end if


      ! Log file
      this%logfile = trim(this%sname) // "-logfile.txt"


      ! Absolute path to log file
      this%abslogfile = trim(this%absfolderout) // trim(this%logfile)


      ! Initializing logger
      call this%logger%init(reload, trim(this%abslogfile))

      call this%logger%print("")
      call this%logger%print("")
      call this%logger%print("  =======  DIFFERENTIAL EVOLUTION PARALLEL PROGRAM  =======  ")
      call this%logger%print("")
      call this%logger%print(this%calendar%get_date() // " : System date")
      call this%logger%print(this%calendar%get_time() // " : System time")
      call this%logger%print("")
      call this%logger%print("")

   end subroutine

end module
