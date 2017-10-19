!> \brief Contains the global variables and subroutines for reading data
!! from input files.
module input

   use mod_global_parameters
   use mod_class_ifile
   use mod_class_system_variables


   implicit none

   ! System
   type(class_system_variables) :: sys_var

   ! Simulation
   integer :: reload    !< upload backup data
   integer :: ind       !< number of the individual
   integer :: ibest     !< index of the best individual in the population
   real(8) :: xfit      !< fitness of the trial individual
   real(8), dimension(:),     allocatable :: x      !< trial individual
   integer :: es        !< exit status (0=success; 1=failure)
   integer :: estatus   !< exit status (0=success; 1=failure)

   ! Evolution history
   character(str_size) :: sname    !< simulations name
   integer :: g         !< generation
   integer :: nu        !< number of unknowns
   integer :: np        !< population size
   integer :: ng        !< maximal number of generations
   real(8), dimension(:),     allocatable :: fit    !< fitness of the population
   real(8), dimension(:,:),   allocatable :: pop    !< population

   ! History vector meaning
   ! hist(ng,np,0:nu) - dimension
   !
   ! hist(g,i,0)    = fitness of individual i of generation g
   ! hist(g,i,1:nu) = coordinates of individual i of generation g

   real(8), dimension(:,:,:), allocatable :: hist   !< history


   ! Domain
   real(8), dimension(:),     allocatable :: xmin   !< lower boundary constraints
   real(8), dimension(:),     allocatable :: xmax   !< higher boundary constraints
   character(10), dimension(:), allocatable :: xname !< names of the unknowns


   ! RSM
   real(8) :: fh        !< Fraction of hybridization
   integer :: rsm_tag   !< Stores the return state of application of DE-RSM

contains

   !============================================================================

   !> \brief Gets the parameters from the input file
   subroutine get_parameters()
      implicit none

      type(class_ifile) :: ifile

      character(20)  :: caux
      integer :: i


      ! Initializing system variables
      call sys_var%init()


      ! Reading the parameters input file

      call ifile%init(filename=trim(sys_var%absfolderin) // trim(sys_var%parfile), field_separator="&")

      call ifile%load()

      call ifile%get_value( sname, "sname")
      call ifile%get_value( reload, "reload")
      call ifile%get_value( fh, "fh")
      call ifile%get_value( nu, "nu")
      call ifile%get_value( np, "np")
      call ifile%get_value( ng, "ng")

      allocate(xmin(nu))
      allocate(xmax(nu))
      allocate(xname(nu))
      allocate(x(nu))
      allocate(fit(np))
      allocate(pop(np,nu))
      allocate(hist(ng,np,0:nu))

      do i = 1, nu
         write(caux,"(A,I1.1,A)") "xname(",i,")"
         call ifile%get_value( xname(i), trim(caux))
         write(caux,"(A,I1.1,A)") "xmin(",i,")"
         call ifile%get_value(  xmin(i), trim(caux))
         write(caux,"(A,I1.1,A)") "xmax(",i,")"
         call ifile%get_value(  xmax(i), trim(caux))
      end do

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
