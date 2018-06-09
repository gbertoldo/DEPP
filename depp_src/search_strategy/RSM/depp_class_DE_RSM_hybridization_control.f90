
!> \brief Provides a class for controlling the hybridization of DE and RSM search strategies

module mod_class_DE_RSM_hybridization_control

   use mod_mpi
   use mod_class_ifile
   use mod_class_system_variables
   use mod_random_generator

   implicit none

   ! Makes everything private, except otherwise stated
   private

   !> \brief A class for controlling the hybridization of DE and RSM search strategies
   type, public :: class_DE_RSM_hybridization_control

      private
      integer ::     np !< Population size
      integer ::     nf !< Number of fitting points for response surface adjustment
      integer ::   ireg !< Index of the current register
      real(8) ::     fh !< Initial factor of hybridization
      real(8) ::  fhmin !< Minimum factor of hybridization
      real(8) ::  fhmax !< Maximum factor of hybridization
      integer ::    fhm !< Model for the dynamical calculation of the factor of hybridization
      character(len=:), allocatable      :: backup_file !< Backup file
      real(8), allocatable, dimension(:) :: r_rsm       !< Register if RSM was applied with success (1) or not (0)

      ! Pointer to system variables
      class(class_system_variables), pointer :: sys_var !< System's variables

   contains

      procedure, pass, public  :: init              !< Constructor
      procedure, pass, public  :: add               !< Add a result
      procedure, pass, public  :: is_rsm_applicable !< Checks if the RSM may be applied
      procedure, pass, private :: rsm_p_success     !< Returns the probability of success of RSM
      procedure, pass, private :: idx               !< Returns the index of the current register
      procedure, pass, public  :: update            !< Calculates the hybridization factor according to a prescribed model
      procedure, pass, private :: save_backup       !< Saves backup of the class state
      procedure, pass, private :: load_backup       !< Loads backup of the class state

   end type

   !> \brief Defines a return code for application of DE-RSM
   type, private :: DE_RSM_RETURN_CODE

      integer ::  DE_APPLIED
      integer :: RSM_APPLIED

   end type

   type(DE_RSM_RETURN_CODE), parameter, public :: DE_RSM_RETURN = DE_RSM_RETURN_CODE(0,1) !< Return code for application of DE-RSM

contains

   !> \brief Constructor
   subroutine init(this, sys_var, np, nf, fh, fhmin, fhmax, fhm)
      implicit none
      class(class_DE_RSM_hybridization_control)         :: this    !< A reference to this object
      class(class_system_variables), target, intent(in) :: sys_var !< System's variables
      integer,                               intent(in) :: np      !< Population size
      integer,                               intent(in) :: nf      !< Number of fitting points for response surface adjustment
      real(8),                               intent(in) :: fh      !< Initial hybridization factor
      real(8),                               intent(in) :: fhmin   !< Minimum hybridization factor
      real(8),                               intent(in) :: fhmax   !< Maximum hybridization factor
      integer,                               intent(in) :: fhm     !< Model for the dynamical calculation of the factor of hybridization

      ! Inner variables
      type(class_ifile) :: ifile
      integer           :: reload


      this%sys_var => sys_var

      this%np = np

      this%nf = nf

      this%fh = fh

      this%fhmin = fhmin

      this%fhmax = fhmax

      this%fhm = fhm

      allocate(this%r_rsm(np))

      this%r_rsm = -1.d0

      this%ireg = 0

      this%backup_file = trim(sys_var%absfolderbkp) // "/class_DE_RSM_hybridization_control_backup.txt"

      ! Reading reload option
      call ifile%init(filename=sys_var%absparfile, field_separator='&')

      call ifile%load()

      call ifile%get_value(reload,"reload")

      if (reload==1) then

         call this%load_backup()

      end if

   end subroutine


   !> \brief Add a result
   subroutine add(this, rsm_tag, xfit, fit)
      implicit none
      class(class_DE_RSM_hybridization_control) :: this    !< A reference to this object
      integer,                       intent(in) :: rsm_tag !< Stores the return state of application of DE-RSM
      real(8),                       intent(in) :: xfit    !< fitness of the trial individual
      real(8),                       intent(in) :: fit     !< fitness of a given individual of the population

      ! Checking DE-RSM status
      select case (rsm_tag)


         ! If DE was applied, do nothing
         case (DE_RSM_RETURN%DE_APPLIED)



         ! If RSM was applied, evaluate the success of the application
         case (DE_RSM_RETURN%RSM_APPLIED)

            ! Getting the index of the current register
            this%ireg = this%idx(this%ireg+1)

            ! If RSM was applied with success
            if ( xfit > fit ) then

               this%r_rsm(this%ireg) = 1.d0

            else

               this%r_rsm(this%ireg) = 0.d0

            end if

         case default

            call this%sys_var%logger%println("class_DE_RSM_hybridization_control: unknown return code. Stopping.")

            call mod_mpi_finalize()

      end select

   end subroutine

   !> \brief Checks if the RSM may be applied
   logical function is_rsm_applicable(this, g)
      implicit none
      class(class_DE_RSM_hybridization_control) :: this !< A reference to this object
      integer,                     intent(in)   :: g    !< Current generation

      ! Inner variables
      real(8) :: rnd ! Random number

      is_rsm_applicable = .false.

      call rand_number(rnd)

      if ( ( 2 * this%nf <= this%np * (g-1)) .and. rnd <= this%fh ) is_rsm_applicable = .true.

   end function


   !> \brief Returns the probability of success of RSM
   real(8) function rsm_p_success(this)
      implicit none
      class(class_DE_RSM_hybridization_control) :: this !< A reference to this object

      ! Inner variables
      integer :: i

      rsm_p_success = -1.d0

      ! Checking if a sufficient number of trials exist for calculating the probability
      do i = 1, this%np

         if ( this%r_rsm(i) < 0.d0 ) return

      end do

      rsm_p_success = sum(this%r_rsm) / dble(this%np)

   end function


   !> \brief Returns the index of the current register
   integer function idx(this, i)
      implicit none
      class(class_DE_RSM_hybridization_control) :: this !< A reference to this object
      integer,                       intent(in) :: i    !< Index of current register

      idx = mod(i,this%np)

      if (idx==0) idx = this%np

   end function



   !> \brief Calculates the hybridization factor according to a prescribed model.
   subroutine update(this)
      implicit none
      class(class_DE_RSM_hybridization_control) :: this !< A reference to this object

      ! Inner variables
      real(8) :: ps

      select case (this%fhm)

         ! Constant hybridization factor
         case (0)

         ! Variable hybridization factor
         case (1)

            ps = this%rsm_p_success()

            if ( 0.d0 <= ps .and. ps <= this%fhmin ) then

               this%fh = this%fhmin

            else if ( this%fhmin < ps .and. ps < this%fhmax ) then

               this%fh =  ps

            else if ( this%fhmax <= ps .and. ps <= 1.d0 ) then

               this%fh =  this%fhmax

            else

            end if

         case default

      end select

      call this%save_backup()

   end subroutine


   !> \brief Saves backup of class state
   subroutine save_backup(this)
      implicit none
      class(class_DE_RSM_hybridization_control) :: this !< A reference to this object

      if (mpio%master) then

         open(20, file=this%backup_file)

         write(20,*) this%ireg
         write(20,*) this%fh
         write(20,*) this%r_rsm

         close(20)

      end if

   end subroutine


   !> \brief Loads backup of class state
   subroutine load_backup(this)
      implicit none
      class(class_DE_RSM_hybridization_control) :: this !< A reference to this object

      open(20, file=this%backup_file)

      read(20,*) this%ireg
      read(20,*) this%fh
      read(20,*) this%r_rsm

      close(20)

   end subroutine

end module
