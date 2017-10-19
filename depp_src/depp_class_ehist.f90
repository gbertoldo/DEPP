
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

      procedure, public, pass :: init

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


      ! Reading the parameters input file

      call ifile%init(filename=trim(sys_var%absfolderin) // trim(sys_var%parfile), field_separator="&")

      call ifile%load()

      call ifile%get_value( this%sname, "sname")
      call ifile%get_value(    this%nu,    "nu")
      call ifile%get_value(    this%np,    "np")
      call ifile%get_value(    this%ng,    "ng")

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

   end subroutine


end module
