
!> \brief Defines a timer class proper to MPI.

module mod_class_timer

   implicit none

   include 'mpif.h'



   ! All members are private by default
   private



   ! Class declaration
   type, public :: class_timer

      private

      real(8) :: t0 = 0.d0
      real(8) :: t1 = 0.d0

   contains

      procedure, public, pass :: start
      procedure, public, pass :: measure
      procedure, public, pass :: elapsed_time
      procedure, public, pass :: formatted_elapsed_time

   end type


   ! Definitions

contains


   !> \brief Starts timer
   subroutine start(this, accumulated)
      class(class_timer)            :: this
      real(8), optional, intent(in) :: accumulated ! Accumulated time from another reading

      if ( present(accumulated) ) then

         this%t0 = MPI_Wtime() + accumulated

      else

         this%t0 = MPI_Wtime()

      end if

      this%t1 = this%t0

   end subroutine


   !> \brief Measure time
   subroutine measure(this)
      class(class_timer) :: this

      this%t1 = MPI_Wtime()

   end subroutine


   !> \brief Returns the elapsed time in seconds
   real(8) function elapsed_time(this)
      class(class_timer) :: this

      elapsed_time = this%t1 - this%t0

   end function


   !> \brief Returns the elapsed time formatted as 00:00:00
   character(len=10) function formatted_elapsed_time(this)
      class(class_timer) :: this

      ! Inner variables
      character(10) :: ftime !< time formatted
      real(8)       :: rtime !< time in seconds
      character(2)  :: char2
      integer       :: hour
      integer       :: min
      integer       :: sec

      rtime = this%elapsed_time()

      hour = int(rtime/3600)
      min = int((rtime - hour*3600)/60)
      sec = int(rtime - hour*3600 - min*60)

      write(ftime, "(i4)") hour

      write(char2, "(i2)") min
      if (min < 10) char2 = "0" // adjustl(char2)
      ftime = trim(ftime) // ":" // char2

      write(char2, "(i2)") sec
      if (sec < 10) char2 = "0" // adjustl(char2)
      ftime = trim(ftime) // ":" // char2

      formatted_elapsed_time = ftime

   end function

end module
