
!> \brief Defines a calendar class

module mod_class_calendar
   implicit none

   ! Makes everything private, except otherwise stated
   private


   !> \brief Defines a calendar class
   type, public :: class_calendar

      private
      character(10) :: date
      character(10) :: time

   contains

      procedure, public, pass :: get_date
      procedure, public, pass :: get_time

   end type



contains



   !> \brief Returns current date
   function get_date(this) result(date)
      implicit none
      class(class_calendar)     :: this
      character(:), allocatable :: date

      call get_date_time(this%date, this%time)

      date = this%date

   end function



   !> \brief Returns current time
   function get_time(this) result(time)
      implicit none
      class(class_calendar)     :: this
      character(:), allocatable :: time

      call get_date_time(this%date, this%time)

      time = this%time

   end function



   !> \brief Obtains system date and time
   subroutine get_date_time(date, time)
      implicit none
      character(10), intent(out) :: date  !< system date
      character(10), intent(out) :: time  !< system time

      character(2)  :: aux1
      character(2)  :: aux2
      character(4)  :: aux3
      character(20) :: vardate   ! system date
      character(20) :: vartime   ! system time
      character(20) :: varzone
      character(50) :: aux
      integer       :: var(8)    ! date and time

      call date_and_time(vardate, vartime, varzone, var)

      write(aux,*) var(3)
      aux1 = trim(adjustl(aux))
      if (var(3) < 10) aux1 = "0" // trim(adjustl(aux1))
      write(aux,*) var(2)
      aux2 = trim(adjustl(aux))
      if (var(2) < 10) aux2 = "0" // trim(adjustl(aux2))
      write(aux,*) var(1)
      aux3 = trim(adjustl(aux))
      date = trim(aux1) // '/' // trim(aux2) // '/' // aux3

      write(aux,*) var(5)
      aux1 = trim(adjustl(aux))
      if (var(5) < 10) aux1 = "0" // trim(adjustl(aux1))
      write(aux,*) var(6)
      aux2 = trim(adjustl(aux))
      if (var(6) < 10) aux2 = "0" // trim(adjustl(aux2))
      write(aux,*) var(7)
      aux3 = trim(adjustl(aux))
      if (var(7) < 10) aux3 = "0" // trim(adjustl(aux3))
      time = trim(aux1) // ':' // trim(aux2) // ':' // aux3

   end subroutine get_date_time


end module
