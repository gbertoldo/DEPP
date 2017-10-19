!> \brief Contains the global variables and subroutines for reading data
!! from input files.
module input

   use mod_global_parameters
   use mod_class_ifile
   use mod_class_system_variables
   use mod_class_ehist

   implicit none

   ! System
   type(class_system_variables) :: sys_var

   ! Evolution history
   type(class_ehist) :: ehist

   ! Simulation
   integer :: reload    !< upload backup data
   integer :: ind       !< number of the individual
   integer :: ibest     !< index of the best individual in the population
   real(8) :: xfit      !< fitness of the trial individual
   real(8), dimension(:),     allocatable :: x      !< trial individual
   integer :: estatus   !< exit status (0=success; 1=failure)

   ! RSM
   real(8) :: fh        !< Fraction of hybridization
   integer :: rsm_tag   !< Stores the return state of application of DE-RSM

contains

   !============================================================================

   !> \brief Gets the parameters from the input file
   subroutine get_parameters()
      implicit none

      type(class_ifile) :: ifile


      ! Initializing system variables
      call sys_var%init()

      call ehist%init(sys_var)

      ! Reading the parameters input file

      call ifile%init(filename=trim(sys_var%absfolderin) // trim(sys_var%parfile), field_separator="&")

      call ifile%load()

      call ifile%get_value( reload, "reload")
      call ifile%get_value( fh, "fh")

      allocate(x(ehist%nu))

   end subroutine get_parameters

   !============================================================================

   ! \brief Loads the backup data
   subroutine load_backup(sys_var, sname, ng, nu, np, tcpu, g, fit, pop, hist)
      implicit none
      class(class_system_variables), intent(in) :: sys_var
      character(len=*), intent(in) :: sname       !< simulations name
      integer, intent(in)  :: ng               !< maximum number of generations
      integer, intent(in)  :: nu               !< dimension of the problem
      integer, intent(in)  :: np               !< size of the population
      integer, intent(out) :: g                !< generation
      real(8), intent(out) :: tcpu             !< accumulated CPU time
      real(8), intent(out) :: fit(np)          !< fitness of the population
      real(8), intent(out) :: pop(np,nu)       !< population of chromosomes
      real(8), intent(out) :: hist(ng,np,0:nu) !< history

      ! Inner variables

      integer :: ind, cg ! Dummy index

      open(23, file = trim(sys_var%absfolderout) // trim(sname) // "-backup.txt")

      read(23,*) tcpu ! " = tcpu:    Accumulated CPU time"

      read(23,*)    g ! " = g:       Last generation"

      read(23,*)

      do ind = 1, np

         read(23,*) fit(ind), pop(ind,:)

      end do

      read(23,*)

      do cg = 1, g

         do ind = 1, np

            read(23,*) hist(cg,ind,:)

         end do

      end do

      read(23,*)

      close(23)

   end subroutine load_backup

end module input
