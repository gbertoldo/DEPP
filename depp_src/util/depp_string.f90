
!> \brief Contains tools to handle strings

module mod_string

   implicit none

   ! Standard string size
   integer, parameter :: str_size = 1000

   ! String conversion from basic types
   interface to_string

      module procedure integer_to_string
      module procedure  real_4_to_string
      module procedure  real_8_to_string

   end interface


contains

   !> \brief Integer to string
   function integer_to_string(val, frmt) result(str)
      implicit none
      integer,                       intent(in)  :: val  !< Value to be converted
      character(len=*), optional,    intent(in)  :: frmt !< Formatter
      character(len=:), allocatable              :: str  !< String

      character(len=str_size) :: tmp

      if ( present(frmt) ) then

         write(tmp,frmt) val

      else

         write(tmp,*) val

      end if

      str = trim(tmp)

   end function


   !> \brief Real(4) to string
   function real_4_to_string(val, frmt) result(str)
      implicit none
      real(4),                       intent(in)  :: val  !< Value to be converted
      character(len=*), optional,    intent(in)  :: frmt !< Formatter
      character(len=:), allocatable              :: str  !< String

      character(len=str_size) :: tmp

      if ( present(frmt) ) then

         write(tmp,frmt) val

      else

         write(tmp,*) val

      end if

      str = trim(tmp)

   end function


   !> \brief Real(8) to string
   function real_8_to_string(val, frmt) result(str)
      implicit none
      real(8),                       intent(in)  :: val  !< Value to be converted
      character(len=*), optional,    intent(in)  :: frmt !< Formatter
      character(len=:), allocatable              :: str  !< String

      character(len=str_size) :: tmp

      if ( present(frmt) ) then

         write(tmp,frmt) val

      else

         write(tmp,*) val

      end if

      str = trim(tmp)

   end function


end module
