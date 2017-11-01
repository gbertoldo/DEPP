
!> \brief Defines a class for printing log information

module mod_class_log_output_control
   implicit none


   ! Makes everything private, except otherwise stated
   private


   ! Initialization flag
   logical :: initialized = .false.


   !> \brief Class for printing log information
   type, public :: class_log_output_control

      private
      integer         :: fileid(2)              !< File ID's

   contains

      procedure, public, pass :: init
      procedure, public, pass :: print
      procedure, public, pass :: finish

   end type


contains


   !> \brief Open files
   subroutine init(this, logfile)
      implicit none
      class(class_log_output_control) :: this
      character(len=*),    intent(in) :: logfile


      ! Initialize only once
      if ( .not. initialized ) then

         initialized = .true.

         ! Standard output (screen)
         this%fileid(1) = 6

         ! Log file
         this%fileid(2) = 100

         open(this%fileid(2),file=logfile)

      end if

   end subroutine



   !> \brief Print messages
   subroutine print(this, msg)
      implicit none
      class(class_log_output_control) :: this
      character(len=*),    intent(in) :: msg

      ! Inner variables
      integer :: i

      do i = 1, size(this%fileid)

         write(this%fileid(i),*) trim(msg)

      end do

   end subroutine



   !> \brief Close files
   subroutine finish(this)
      implicit none
      class(class_log_output_control) :: this

      ! Inner variables
      integer :: i

      do i = 2, size(this%fileid)

         close(this%fileid(i))

      end do

   end subroutine


end module
