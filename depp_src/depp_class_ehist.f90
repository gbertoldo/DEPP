
!> \brief Provides a class for generating evolution history objects
!! These objects must contain information about the population, its fitness,
!! current generation etc.

module mod_class_ehist

   use mod_global_parameters
   use mod_class_system_variables
   use mod_class_ifile

   implicit none

  ! Makes everything private, except otherwise stated
   private

   ! Evolution history class
   type, public :: class_ehist

      character(str_size) :: sname                     !< Simulation name
      integer :: g                                     !< Current generation
      integer :: nu                                    !< Number of unknowns
      integer :: np                                    !< Population size
      integer :: ng                                    !< Maximum number of generations
      integer :: ibest                                 !< Index of the best individual in the population
      real(8) :: tcpu                                  !< CPU time
      real(8), dimension(:),     allocatable :: fit    !< Fitness of the current population
      real(8), dimension(:,:),   allocatable :: pop    !< Current population


      ! History vector meaning
      ! hist(ng,np,0:nu) - dimension
      !
      ! hist(g,i,0)    = fitness of individual i of generation g
      ! hist(g,i,1:nu) = coordinates of trial individual i of generation g

      real(8), dimension(:,:,:), allocatable :: hist   !< history


      ! Domain
      real(8),       dimension(:), allocatable :: xmin  !< lower boundary constraints
      real(8),       dimension(:), allocatable :: xmax  !< higher boundary constraints
      character(10), dimension(:), allocatable :: xname !< names of the unknowns

   contains

      procedure, public,  pass :: init
      procedure, private, pass :: reload
      procedure, public,  pass :: save_backup

   end type

contains


   !> \brief Constructor
   subroutine init(this, sys_var)
      implicit none
      class(class_ehist)                        :: this
      class(class_system_variables), intent(in) :: sys_var


      ! Inner variables
      type(class_ifile) :: ifile
      character(20)     :: caux
      integer           :: i
      integer           :: reload


      ! Reading the parameters input file

      call ifile%init(filename=trim(sys_var%absfolderin) // trim(sys_var%parfile), field_separator="&")

      call ifile%load()

      call ifile%get_value( this%sname,  "sname")
      call ifile%get_value(    this%nu,     "nu")
      call ifile%get_value(    this%np,     "np")
      call ifile%get_value(    this%ng,     "ng")
      call ifile%get_value(     reload, "reload")

      allocate(this%xmin(this%nu))
      allocate(this%xmax(this%nu))
      allocate(this%xname(this%nu))
      allocate(this%fit(this%np))
      allocate(this%pop(this%np,this%nu))
      allocate(this%hist(this%ng,this%np,0:this%nu))

      do i = 1, this%nu

         write(caux,"(A,I1.1,A)") "xname(",i,")"
         call ifile%get_value( this%xname(i), trim(caux))

         write(caux,"(A,I1.1,A)") "xmin(",i,")"
         call ifile%get_value(  this%xmin(i), trim(caux))

         write(caux,"(A,I1.1,A)") "xmax(",i,")"
         call ifile%get_value(  this%xmax(i), trim(caux))

      end do

      this%tcpu = 0.d0


      ! If reload=0, data is initialized, otherwise the population and its fitness are read from the backup file
      if ( reload == 0 ) then

         this%g     =  0
         this%pop   =  0.d0
         this%fit   = -huge(1.d0)
         this%hist  = 0.d0
         this%ibest = 1

      else

         ! Loading data
         call this%reload(sys_var)

      end if


   end subroutine




   ! \brief Loads the backup data
   subroutine reload(this, sys_var)
      implicit none
      class(class_ehist) :: this
      class(class_system_variables), intent(in) :: sys_var

      ! Inner variables

      open(23, file = trim(sys_var%absfolderout) // trim(this%sname) // "-ehist-backup.txt")

      read(23,*) this%sname
      read(23,*) this%g
      read(23,*) this%nu
      read(23,*) this%np
      read(23,*) this%ng
      read(23,*) this%ibest
      read(23,*) this%tcpu
      read(23,*) this%fit
      read(23,*) this%pop
      read(23,*) this%hist
      read(23,*) this%xmin
      read(23,*) this%xmax
      read(23,*) this%xname

      close(23)

   end subroutine reload





   ! \brief Saves a backup
   subroutine save_backup(this, sys_var)
      implicit none
      class(class_ehist) :: this
      class(class_system_variables), intent(in) :: sys_var

      open(23, file = trim(sys_var%absfolderout) // trim(this%sname) // "-ehist-backup.txt")

      write(23,*) this%sname
      write(23,*) this%g
      write(23,*) this%nu
      write(23,*) this%np
      write(23,*) this%ng
      write(23,*) this%ibest
      write(23,*) this%tcpu
      write(23,*) this%fit
      write(23,*) this%pop
      write(23,*) this%hist
      write(23,*) this%xmin
      write(23,*) this%xmax
      write(23,*) this%xname

      close(23)

   end subroutine save_backup


end module
