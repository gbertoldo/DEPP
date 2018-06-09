
!> \brief Implements a class that uses an external program to calculate the fitness function

module mod_class_external_fitness_calculator

   use mod_mpi
   use mod_class_ifile
   use mod_class_ehist
   use mod_class_system_variables
   use mod_class_abstract_fitness_calculator
   use mod_class_fitness_calculator_exit_code

   implicit none

   ! Makes everything private, except otherwise stated
   private

   !> \brief Implements the fitness calculator abstract class for calculations from an external program
   type, public, extends(class_abstract_fitness_calculator) :: class_external_fitness_calculator

      private

      ! Store the number of success and failures of each thread
      integer, allocatable, dimension(:) :: success_counter_thread !< Thread number of success
      integer, allocatable, dimension(:) :: failure_counter_thread !< Thread number of failures

      ! Store the number of success and failures of all threads
      integer                            :: success_counter !< Total number of success
      integer                            :: failure_counter !< Total number of failures

      integer                            :: verbosity       !< Verbosity level for log

      character(len=:), allocatable :: backup_file  !< Backup file
      integer                       :: np           !< Population size
      character(len=:), allocatable :: sname        !< Simulation name
      character(len=:), allocatable :: absfolderout !< Absolute path to output folder
      character(len=:), allocatable :: fdir         !< Name of working directory for fitness calculation
      character(len=:), allocatable :: ffit         !< Name of executable for fitness calculation

   contains

      procedure, public,  pass :: init              !< Constructor
      procedure, public,  pass :: get_fitness       !< Calculates the fitness of a trial individual
      procedure, public,  pass :: statistics_info   !< Returns a string with fitness calculator statistics information
      procedure, private, pass :: save_failure      !< Register failures in calculations of fitness
      procedure, public,  pass :: data_size         !< Size of the vector to be shared among threads
      procedure, public,  pass :: send              !< Sends information from this threat
      procedure, public,  pass :: recv              !< Receives information from other threads
      procedure, public,  pass :: update            !< Updates class after a parallel computation cycle
      procedure, private, pass :: save_backup       !< Saves a backup of the class state
      procedure, private, pass :: load_backup       !< Loads a backup of the class state


   end type


contains


   !> \brief Constructor
   subroutine init(this, sys_var, ehist)
      implicit none
      class(class_external_fitness_calculator)   :: this    !< A reference to this object
      class(class_system_variables), intent(in)  :: sys_var !< System's variables
      class(class_ehist),            intent(in)  :: ehist   !< Evolution history

      ! Inner variables
      type(class_ifile) :: ifile
      integer           :: reload

      this%absfolderout = sys_var%absfolderout
      this%fdir         = sys_var%fdir
      this%ffit         = sys_var%ffit
      this%sname        = ehist%sname

      ! Backup file
      this%backup_file = trim(sys_var%absfolderbkp) // "/class_external_fitness_calculator_backup.txt"

      ! Population size
      this%np = ehist%np

      ! Allocating resources
      allocate(this%success_counter_thread(ehist%np))
      allocate(this%failure_counter_thread(ehist%np))

      ! Reading reload option
      call ifile%init(filename=sys_var%absparfile, field_separator='&')

      call ifile%load()

      call ifile%get_value(reload,"reload")

      if (reload==0) then

         this%success_counter_thread = 0
         this%failure_counter_thread = 0

         this%success_counter = 0
         this%failure_counter = 0

      else

         call this%load_backup()

      end if

      call ifile%get_value(this%verbosity,"verbosity")

   end subroutine


   !> \brief Calculates the fitness function
   subroutine get_fitness(this, ehist, i, x, fit, ecode)
      implicit none
      class(class_external_fitness_calculator) :: this  !< A reference to this object
      class(class_ehist),          intent(in)  :: ehist !< Evolution history
      integer,                     intent(in)  :: i     !< Index of the trial individual
      real(8), dimension(:),       intent(in)  :: x     !< Trial individual
      real(8),                     intent(out) :: fit   !< Fitness of the trial individual
      integer,                     intent(out) :: ecode !< Exit code

      ! Inner variables
      integer        :: j
      character(3)   :: char3
      character(200) :: arqpar   ! name of file for the parameters
      character(200) :: arqfit   ! name of file for the fitness


      ! Generating the file's names
      write(char3,"(I3.3)") i

      arqpar = trim(this%absfolderout) // "parameters" // char3 // ".txt"
      arqfit = trim(this%absfolderout) // "fitness"    // char3 // ".txt"


      ! Creating the file with the trial individual
      open(27, file = trim(arqpar))

      if (len(trim(arqfit)) <= 23) then
         write(27,"(a23, a)") "'" // trim(arqfit) // "'", " = arqfit: name of the file for fitness"
      else
         write(27,"(a  , a)") "'" // trim(arqfit) // "'", " = arqfit: name of the file for fitness"
      end if

      write(27,"(i23, a)") ehist%nu, " = nu:    number of unknowns"

      do j = 1, ehist%nu
         write(27,"(1pe23.15, a36)") x(j)
      end do

      write(27,*)
      close(27)


      ! Calling the external program for fitness calculation
      call execute_command_line("(cd " // trim(this%fdir) // " && exec ./" &
                                // trim(this%ffit) // " " // trim(arqpar) // ") " // " > /dev/null")

      ! Reading the solution
      open(26, file = trim(arqfit))
      read(26,*) fit
      read(26,*) ecode
      close(26)


      ! For the first generation, if the fitness calculation fails, give up is not allowed.
      ! So it is necessary to try again.
      if (ehist%g==1 .and. ecode==fitness_calculator_exit_code%FAIL_AND_GIVE_UP) then

         ecode = fitness_calculator_exit_code%FAIL_AND_TRY_AGAIN

      end if


      ! In case of failure, saves the information to a file
      select case (ecode)

          case (  fitness_calculator_exit_code%FAIL_AND_GIVE_UP    &
              :   fitness_calculator_exit_code%FAIL_AND_TRY_AGAIN  )

            call this%save_failure(ehist, i, x, fit, ecode)

            this%failure_counter_thread(i) = this%failure_counter_thread(i) + 1

          case default

            this%success_counter_thread(i) = this%success_counter_thread(i) + 1

      end select


   end subroutine


   !> \brief Returns a string with statistics about fitness calculator
   function statistics_info(this) result(info)
      implicit none
      class(class_external_fitness_calculator) :: this !< A reference to this object
      character(len=:), allocatable            :: info !< A string with statistics about fitness calculator

      ! Inner variables
      character        :: ENDL = char(10) ! New line char
      character(len=7) :: caux

      select case (this%verbosity)
          case (0)

            info = ""

          case (1:2)

            info = ENDL // ENDL // "Fitness calculator statistics: " // ENDL

            write(caux,"(I7)") this%success_counter

            info = info // caux // " = number of function calls returning success" // ENDL

            write(caux,"(I7)") this%failure_counter

            info = info // caux // " = number of function calls returning failure" // ENDL

          case default

            info = ""

      end select

   end function


   !> \brief Saves the failure to a file
   subroutine save_failure(this, ehist, i, x, fit, ecode)
      implicit none
      class(class_external_fitness_calculator) :: this  !< A reference to this object
      class(class_ehist),           intent(in) :: ehist !< Evolution history
      integer,                      intent(in) :: i     !< Index of the trial individual
      real(8), dimension(:),        intent(in) :: x     !< Trial individual
      real(8),                      intent(in) :: fit   !< Fitness of the trial individual
      integer,                      intent(in) :: ecode !< Exit code

      ! Inner variables

      integer        :: j    ! Dummy index
      character(3)   :: str1 ! Auxiliary string 1
      character(3)   :: str2 ! Auxiliary string 2

      character(len=:), allocatable :: fout ! Output file name


      ! Creating the name of the output file
      write(str1,"(I3.3)") ehist%g
      write(str2,"(I3.3)") i

      fout = trim(this%absfolderout) // "fitness_failures_i" // str2 // ".txt"


      ! Saving data
      open(10, file = fout, position = "append" )

      if (len(trim(adjustl(this%sname))) > 23) then
         write(10,*) trim(adjustl(this%sname)), " = sname:    simulation name"
      else
         write(10,"(A23,A)") trim(adjustl(this%sname)), " = sname:    simulation name"
      end if

      write(10,"(I23,A)") ehist%g, " =     g:  generation number"
      write(10,"(I23,A)")       i, " =   ind:  individual number"
      write(10,"(I23,A)")   ecode, " = ecode:  error code"

      do j = 1, ehist%nu

         write(10,"(ES23.16,A,I2,A)") x(j), " = x(", j, "):"

      end do

      write(10,*)

      close(10)

   end subroutine


   !> \brief Size of the vector to be shared among threads
   integer function data_size(this)
      implicit none
      class(class_external_fitness_calculator) :: this !< A reference to this object

      data_size = this%np

   end function


   !> \brief Sends information from this threat
   subroutine send(this, i, to_thread)
      implicit none
      class(class_external_fitness_calculator) :: this      !< A reference to this object
         integer,                   intent(in) :: i         !< Index of the shared vector
         integer,                   intent(in) :: to_thread !< Receiver thread

      call mod_mpi_send(to_thread, this%success_counter_thread(i) )
      call mod_mpi_send(to_thread, this%failure_counter_thread(i) )

   end subroutine


   !> \brief Receives information from other threads
   subroutine recv(this, i, from_thread)
      implicit none
      class(class_external_fitness_calculator) :: this        !< A reference to this object
      integer,                      intent(in) :: i           !< Index of the shared vector
      integer,                      intent(in) :: from_thread !< Sender thread

      call mod_mpi_recv(from_thread, this%success_counter_thread(i) )
      call mod_mpi_recv(from_thread, this%failure_counter_thread(i) )

   end subroutine


   !> \brief Updates class after a parallel computation cycle
   subroutine update(this)
      implicit none
      class(class_external_fitness_calculator) :: this !< A reference to this object

      this%success_counter = sum(this%success_counter_thread)
      this%failure_counter = sum(this%failure_counter_thread)

      call this%save_backup()

   end subroutine


   !> \brief Saves a backup of the class state
   subroutine save_backup(this)
      implicit none
      class(class_external_fitness_calculator) :: this !< A reference to this object

      if (mpio%master) then

         open(20, file=this%backup_file)

         write(20,*) this%success_counter_thread
         write(20,*) this%failure_counter_thread
         write(20,*) this%success_counter
         write(20,*) this%failure_counter

         close(20)

      end if

   end subroutine


   !> \brief Loads a backup of the class state
   subroutine load_backup(this)
      implicit none
      class(class_external_fitness_calculator) :: this !< A reference to this object

      open(20, file=this%backup_file)

      read(20,*) this%success_counter_thread
      read(20,*) this%failure_counter_thread
      read(20,*) this%success_counter
      read(20,*) this%failure_counter

      close(20)

   end subroutine


end module
