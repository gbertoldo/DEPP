!> \brief Contains the global variables and subroutines for reading data
!! from input files.
module input

   implicit none

   character(600) :: folderin = "./depp_input/"   !< folder the for input files
   character(600) :: folderout = "./depp_output/" !< folder the for output files
   character(600) :: logfile  !< name of the log file
   character(600) :: sname    !< simulations name
   character(600) :: fdir     !< name of working directory for fitness calculation
   character(600) :: ffit     !< name of executable for fitness calculation

   integer :: iarq = 10 !< number of the input file
   integer :: reload    !< upload backup data
   integer :: ind       !< number of the individual
   integer :: g         !< generation
   integer :: nu        !< number of unknowns
   integer :: np        !< population size
   integer :: ng        !< maximal number of generations
   integer :: GNoAcc    !< maximum number of generations allowed before stopping if no improvement was found
   integer :: nf        !< number of individuals for fitting RSM
   integer :: kss       !< kind of search strategy
   integer :: kh        !< kind of hybridization (see input file)
   integer :: kw        !< kind of weighting function for RSM fitting
   integer :: kpm       !< kind of population convergence measure
   integer :: ibest     !< index of the best individual in the population
   integer :: r(3)      !< indexes of selected individuals

   integer :: clock     !< clock time for seeds generation
   integer :: seed(97)  !< seeds for random numbers

   integer :: es        !< exit status (0=success; 1=failure)
   integer :: estatus   !< exit status (0=success; 1=failure)

   integer :: iproc     !< mpi: identification number of the process
   integer :: code      !< mpi: status code
   integer :: nproc     !< mpi: number of processes
   integer :: tag       !< mpi: message label
   integer :: comm      !< mpi: MPI communicator

   real(8) :: dif       !< differentiation constant
   real(8) :: crs       !< crossover constant
   real(8) :: crsh      !< crossover constant of the hybridized model
   real(8) :: xfit      !< fitness of the trial individual
   real(8) :: tcpu      !< total CPU time
   real(8) :: tcpu0     !< accumulated CPU time
   real(8) :: tcpu1     !< starts CPU time
   real(8) :: tcpu2     !< finishes CPU time
   real(8) :: fnb       !< Multiple of the minimum number of points for RSM fitting
   real(8) :: fh        !< Fraction of hybridization
   real(8) :: fhmin     !< Minimum fraction of hybridization
   real(8) :: fhmax     !< Maximum fraction of hybridization
   integer :: fhm       !< Model for the dynamical calculation of the factor of hybridization
   integer :: rsm_tag   !< Stores the return state of application of DE-RSM

   real(8) :: detol     !< tolerance for the convergence measure in the DE algorithm

   integer :: nstp      !< Number of trials for adjusting the step of the RSM solution
   real(8) :: netol     !< Tolerance for distance when selecting neighbors points for RSM adjusting

   logical :: stopflag  !< Flag indicating that stopping condition was reached


   real(8), dimension(:),     allocatable :: x      !< trial individual
   real(8), dimension(:),     allocatable :: xmin   !< lower boundary constraints
   real(8), dimension(:),     allocatable :: xmax   !< higher boundary constraints
   real(8), dimension(:),     allocatable :: fit    !< fitness of the population
   real(8), dimension(:,:),   allocatable :: pop    !< population

   ! History vector meaning
   ! hist(ng,np,0:nu) - dimension
   !
   ! hist(g,i,0)    = fitness of individual i of generation g
   ! hist(g,i,1:nu) = coordinates of individual i of generation g

   real(8), dimension(:,:,:), allocatable :: hist   !< history

   character(10), dimension(:), allocatable :: xname !< names of the unknowns

contains

   !============================================================================

   !> \brief Gets the parameters from the input file
   subroutine get_parameters(   folderin, folderout, sname, iarq, reload, fdir,     &
         ffit, tcpu0, kss, kh, fh, fhmin, fhmax, fhm, fnb, kw, kpm, nu, np, ng,     &
         GNoAcc, dif, crs, crsh, nstp, netol, detol, xmin, xmax, xname, x, fit,     &
         pop, hist)
      implicit none
      character(len=*), intent(inout) :: folderin  !< folder the for input files
      character(len=*), intent(inout) :: folderout !< folder the for output files
      character(len=*), intent(out) :: sname       !< simulations name
      character(len=*), intent(out) :: fdir        !< name of working directory for fitness calculation
      character(len=*), intent(out) :: ffit        !< name of executable for fitness calculation
      integer, intent(inout) :: iarq   !< number of input file
      integer, intent(out) :: reload   !< upload backup data
      integer, intent(out) :: kss      !< kind of search strategy
      integer, intent(out) :: kh       !< kind of the hybridization (see input file)
      real(8), intent(out) :: fh       !< Fraction of hybridization
      real(8), intent(out) :: fhmin    !< Minimum hybridization factor
      real(8), intent(out) :: fhmax    !< Maximum hybridization factor
      integer, intent(out) :: fhm      !< Model for the dynamical calculation of the factor of hybridization
      real(8), intent(out) :: fnb      !< Multiple of the minimum number of points for RSM fitting
      integer, intent(out) :: kw       !< kind of weighting function for RSM fitting
      integer, intent(out) :: kpm      !< kind of population convergence measure
      integer, intent(out) :: nu       !< number of unknowns
      integer, intent(out) :: np       !< population size
      integer, intent(out) :: ng       !< maximal number of generations
      integer, intent(out) :: GNoAcc   !< maximum number of generations allowed before stopping if no improvement was found
      real(8), intent(out) :: tcpu0    !< accumulated CPU time
      real(8), intent(out) :: dif      !< differentiation constant
      real(8), intent(out) :: crs      !< crossover constant
      real(8), intent(out) :: crsh     !< crossover constant of the hybridized model
      integer, intent(out) :: nstp     !< Number of trials for adjusting the step of the RSM solution
      real(8), intent(out) :: netol    !< Tolerance for distance when selecting neighbors points for RSM adjusting
      real(8), intent(out) :: detol    !< tolerance for the convergence measure in the DE algorithm
      real(8),       dimension(:),     allocatable, intent(out) :: x     !< trial individual
      real(8),       dimension(:),     allocatable, intent(out) :: xmin  !< lower boundary constraints
      real(8),       dimension(:),     allocatable, intent(out) :: xmax  !< higher boundary constraints
      character(10), dimension(:),     allocatable, intent(out) :: xname !< names of the unknowns
      real(8),       dimension(:),     allocatable, intent(out) :: fit   !< fitness of the population
      real(8),       dimension(:,:),   allocatable, intent(out) :: pop   !< population
      real(8),       dimension(:,:,:), allocatable, intent(out) :: hist  !< history

      character(400) :: CWD      !< current working directory
      character(200) :: arqin    !< input file name
      integer :: i

      ! Getting
      call getcwd(CWD)

      ! Complete path of input and output
      folderin  = trim(CWD) // "/" // trim(folderin)
      folderout = trim(CWD) // "/" // trim(folderout)


      open(10, file = trim(folderin) // "input_file.txt")
      read(10,*) arqin
      close(10)

      open(iarq, file = trim(folderin) // trim(adjustl(arqin)))

      read(iarq,*) sname
      read(iarq,*) reload

      folderout = trim(folderout) // trim(sname) // "/"

      tcpu0 = 0.d0

      read(iarq,*) fdir
      read(iarq,*) ffit
      read(iarq,*) kss
      read(iarq,*) kh
      read(iarq,*) fh
      read(iarq,*) fhmin
      read(iarq,*) fhmax
      read(iarq,*) fhm
      read(iarq,*) fnb
      read(iarq,*) kw
      read(iarq,*) kpm
      read(iarq,*) nu
      read(iarq,*) np
      read(iarq,*) ng
      read(iarq,*) GNoAcc
      read(iarq,*) dif
      read(iarq,*) crs
      read(iarq,*) crsh
      read(iarq,*) nstp
      read(iarq,*) netol
      read(iarq,*) detol

      allocate(xmin(nu))
      allocate(xmax(nu))
      allocate(xname(nu))
      allocate(x(nu))
      allocate(fit(np))
      allocate(pop(np,nu))
      allocate(hist(ng,np,0:nu))

      do i = 1, nu
         read(iarq,*) xname(i)
         read(iarq,*) xmin(i)
         read(iarq,*) xmax(i)
      end do

      close(10)

   end subroutine get_parameters

   !============================================================================

   ! \brief Loads the backup data
   subroutine load_backup(folderout, sname, ng, nu, np, tcpu, g, fit, pop, hist)
      implicit none
      character(len=*), intent(in) :: folderout   !< folder the for output files
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

      open(23, file = trim(folderout) // trim(sname) // "-backup.txt")

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
