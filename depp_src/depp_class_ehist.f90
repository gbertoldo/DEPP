
!> \brief Provides a class for generating evolution history objects.
!! These objects must contain information about the population, its fitness,
!! current generation etc.

module mod_class_ehist

   use mod_mpi
   use mod_string
   use mod_class_system_variables
   use mod_class_ifile

   implicit none

  ! Makes everything private, except otherwise stated
   private

   !> \brief Evolution history class
   type, public :: class_ehist

      character(str_size) :: sname                     !< Simulation name
      integer :: g                                     !< Current generation
      integer :: nu                                    !< Number of unknowns
      integer :: np                                    !< Population size
      integer :: ng                                    !< Maximum number of generations
      integer :: ibest                                 !< Index of the best individual in the population
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
      real(8),       dimension(:), allocatable :: xmax  !< upper boundary constraints

   contains

      procedure, public,  pass :: init                                 !< Constructor
      procedure, public,  pass :: new_generation                       !< Starts a new generation
      procedure, public,  pass :: add_trial_population                 !< Add a trial population to evolution history
      procedure, public,  pass :: select_individuals                   !< Selects the individuals of the trial population
      procedure, public,  pass :: save_backup                          !< Saves a backup
      procedure, private, pass :: load_backup                          !< Loads a backup
      procedure, public,  pass :: trial_population_info                !< Returns a string with trial population information
      procedure, public,  pass :: current_population_info              !< Returns a string with current population information
      procedure, public,  pass :: current_population_statistics_info   !< Returns a string with evolution statistics information
      procedure, public,  pass :: final_solution_info                  !< Returns a string with the final solution information
      procedure, public,  pass :: info                                 !< Returns a string with current generation information

   end type

contains


   !> \brief Constructor
   subroutine init(this, sys_var)
      implicit none
      class(class_ehist)                        :: this    !< A reference to this object
      class(class_system_variables), intent(in) :: sys_var !< System's variables


      ! Inner variables
      type(class_ifile)       :: ifile
      integer                 :: reload
      character(len=str_size) :: str


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
      allocate(this%fit(this%np))
      allocate(this%pop(this%np,this%nu))
      allocate(this%hist(this%ng,this%np,0:this%nu))

      call ifile%get_value( str, "lower_bound")
      call get_constraint_vector(str, this%xmin)

      call ifile%get_value( str, "upper_bound")
      call get_constraint_vector(str, this%xmax)


      ! If reload=0, data is initialized, otherwise the population and its fitness are read from the backup file
      if ( reload == 0 ) then

         this%g     =  0
         this%pop   =  0.d0
         this%fit   = -huge(1.d0)
         this%hist  = 0.d0
         this%ibest = 1

      else

         ! Loading data
         call this%load_backup(sys_var)

      end if

   contains

      !> \brief Reads the constraint vectors
      subroutine get_constraint_vector(str, bound_vector)
         implicit none
         character(len=*),       intent(in) :: str          !< String containing the data
         real(8), dimension(:), intent(out) :: bound_vector !< The vector with the constraints

         ! Auxiliary variables
         integer :: i
         integer :: IO1
         integer :: IO2
         real(8) :: raux

         ! Trying to read an augmented bound vector (just to check the vector size)
         read(str,*,IOStat=IO1) bound_vector, raux

         ! If no error occured, the input data is wrong
         if (IO1==0) then

            call sys_var%logger%print("class_ehist: Unable to read constraint vectors. Stopping...")

            call mod_mpi_finalize()

         end if

         ! Trying to read bound vector
         read(str,*,IOStat=IO1) bound_vector

         ! If an error occured, try to discover the number of elements of this vector
         if (IO1/=0) then

            do i = this%nu-1, 1, -1

               read(str,*,IOStat=IO2) bound_vector(1:i)

               if ( IO2==0 ) exit

            end do

            ! If there is only one element, then all elements are equal to this one.
            if ( i == 1 .and. IO2 == 0) then

               bound_vector = bound_vector(1)

            ! Otherwise, there is some error...
            else

               call sys_var%logger%print("class_ehist: Unable to read constraint vectors. Stopping...")

               call mod_mpi_finalize()

            end if

         end if

      end subroutine


   end subroutine



   !> \brief Adds a new generation to evolution history
   subroutine new_generation(this)
      implicit none
      class(class_ehist) :: this !< A reference to this object

      this%g = this%g + 1

   end subroutine



   !> \brief Add a trial population to evolution history
   subroutine add_trial_population(this, pop, fit)
      implicit none
      class(class_ehist)                  :: this !< A reference to this object
      real(8), dimension(:,:), intent(in) :: pop  !< Trial individuals population
      real(8), dimension(:),   intent(in) :: fit  !< Fitness of the population

      ! Inner variables
      integer :: i

      ! Updating history

      do i = 1, this%np

         this%hist(this%g,i,1:this%nu) = pop(i,:)  ! Trial population
         this%hist(this%g,i,        0) = fit(i)    ! Fitness of the trial population

      end do

   end subroutine



   !> \brief Selects the best individuals
   subroutine select_individuals(this)
      implicit none
      class(class_ehist) :: this !< A reference to this object

      ! Inner variables
      integer :: i
      real(8) :: xfit

      ! For each individual of the population
      do i = 1, this%np

         xfit = this%hist(this%g,i,0)

         ! Selecting the best individual
         if ( xfit >= this%fit(i)) then

            this%pop(i,:) = this%hist(this%g,i,1:)

            this%fit(i)   = xfit

            if ( xfit >= this%fit(this%ibest)) this%ibest = i

         end if

      end do

   end subroutine



   ! \brief Saves a backup
   subroutine save_backup(this, sys_var)
      implicit none
      class(class_ehist)                        :: this    !< A reference to this object
      class(class_system_variables), intent(in) :: sys_var !< System's variables

      if (mpio%master) then

         open(23, file = trim(sys_var%absfolderbkp) // trim(this%sname) // "-ehist-backup.txt")

         write(23,*) this%sname
         write(23,*) this%g
         write(23,*) this%nu
         write(23,*) this%np
         write(23,*) this%ng
         write(23,*) this%ibest
         write(23,*) this%fit
         write(23,*) this%pop
         write(23,*) this%hist
         write(23,*) this%xmin
         write(23,*) this%xmax

         close(23)

      end if

   end subroutine



   ! \brief Loads the backup data
   subroutine load_backup(this, sys_var)
      implicit none
      class(class_ehist)                        :: this    !< A reference to this object
      class(class_system_variables), intent(in) :: sys_var !< System's variables

      ! Inner variables

      open(23, file = trim(sys_var%absfolderbkp) // trim(this%sname) // "-ehist-backup.txt")

      read(23,*) this%sname
      read(23,*) this%g
      read(23,*) this%nu
      read(23,*) this%np
      read(23,*) this%ng
      read(23,*) this%ibest
      read(23,*) this%fit
      read(23,*) this%pop
      read(23,*) this%hist
      read(23,*) this%xmin
      read(23,*) this%xmax

      close(23)

   end subroutine



   !> \brief Returns a string with trial population information
   function trial_population_info(this) result(str)
      implicit none
      class(class_ehist)            :: this !< A reference to this object
      character(len=:), allocatable :: str  !< Information string

      ! Inner variables
      integer                       :: i
      character                     :: endl=char(10) ! End line char
      character(len=100000)         :: caux1
      character(len=:), allocatable :: caux2


      caux1 = ""
      caux2 = endl // endl

      write(caux1,*) caux2, "Generation: ", this%g, endl, "Trial population:"
      caux2 = trim(caux1) // endl

      do i = 1, this%np

         write(caux1,"(a, a, i4, a, 10(1pe23.15, 2x))") trim(caux2) &
         , "The performance of the",  i, "th individual is ", this%hist(this%g,i,0), this%hist(this%g,i,1:)

         caux2 = trim(caux1) // endl

      end do

      str = trim(caux2)

   end function



   !> \brief Returns a string with current population information
   function current_population_info(this) result(str)
      implicit none
      class(class_ehist)            :: this !< A reference to this object
      character(len=:), allocatable :: str  !< Information string

      ! Inner variables
      integer                       :: i
      character                     :: endl=char(10) ! End line char
      character(len=100000)         :: caux1
      character(len=:), allocatable :: caux2


      caux1 = ""
      caux2 = endl // endl

      write(caux1,*) caux2, "Generation: ", this%g, endl, "Current population:"
      caux2 = trim(caux1) // endl

      do i = 1, this%np

         write(caux1,"(a, a, i4, a, 10(1pe23.15, 2x))") trim(caux2) &
         , "The performance of the",  i, "th individual is ", this%fit(i), this%pop(i,:)

         caux2 = trim(caux1) // endl

      end do

      str = trim(caux2)

   end function



   !> \brief Returns a string with current population statistics information
   function current_population_statistics_info(this) result(str)
      implicit none
      class(class_ehist)            :: this !< A reference to this object
      character(len=:), allocatable :: str  !< Information string

      ! Inner variables
      character                     :: endl=char(10) ! End line char
      character(len=100000)         :: caux1
      character(len=:), allocatable :: caux2

      caux1 = ""
      caux2 = endl // "Current population statistics:" // endl

      write(caux1,"(A, A, 1(2x, 1pe23.15))") caux2, "   --->  Mean fitness: ", sum(this%fit)/this%np

      caux2 = trim(caux1) // endl

      write(caux1,"(A, A, 1(2x, 1pe23.15))") caux2, "   --->  Max. fitness: ", maxval(this%fit)

      caux2 = trim(caux1) // endl

      str = caux2

   end function



   !> \brief Returns final solution information
   function final_solution_info(this) result(str)
      implicit none
      class(class_ehist)            :: this !< A reference to this object
      character(len=:), allocatable :: str  !< Information string

      ! Inner variables
      integer                       :: j
      character                     :: endl=char(10) ! End line char
      character(len=100000)         :: caux1
      character(len=:), allocatable :: caux2


      caux1 = ""
      caux2 = endl // endl // endl

      write(caux1,*) caux2, " ====================  SOLUTION  ===================== "

      caux2 = trim(caux1) // endl // endl


      do j = 1, this%nu

         write(caux1,"(a, 1pe23.15, a, i2, a, i2, a)") caux2, this%pop(this%ibest,j), &
            " = x(", j, "):    The best value for the ", j, " unknown"

         caux2 = trim(caux1) // endl

      end do

      caux2 = caux2 // endl // endl

      write(caux1,"(a, 1pe23.15, a)") caux2, this%fit(this%ibest), " = fittest: The best fitness found"

      caux2 = trim(caux1) // endl // endl

      write(caux1,"(a,I23, a)") caux2, this%g, " = g:       Final number of generations"

      caux2 = trim(caux1) // endl

      str = caux2

   end function



   !> \brief Returns a string with current generation information
   function info(this) result(str)
      implicit none
      class(class_ehist)            :: this !< A reference to this object
      character(len=:), allocatable :: str  !< Information string


      str = this%trial_population_info()                 &
         // this%current_population_info()               &
         // this%current_population_statistics_info()

   end function


end module
