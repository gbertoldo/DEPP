!    DEPP - Differential Evolution Parallel Program
!
!    Copyright (C) 2019 by Jonas Joacir Radtke, Guilherme Bertoldo and Carlos Henrique Marchi
!
!    This program is free software: you can redistribute it and/or modify
!    it under the terms of the GNU General Public License as published by
!    the Free Software Foundation, either version 3 of the License, or
!    (at your option) any later version.
!
!    This program is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU General Public License for more details.
!
!    You should have received a copy of the GNU General Public License
!    along with this program.  If not, see <http://www.gnu.org/licenses/>.
!    
!    Contact:
!          Jonas Joacir Radtke (a)
!                 E-mail: jonas.radtke@gmail.com
!
!          Guilherme Bertoldo (a)
!                 E-mail: glbertoldo@gmail.com
!
!          Carlos Henrique Marchi (b)
!                 E-mail: chmcfd@gmail.com
!    Institution
!          (a) Federal University of Technology - Paraná - UTFPR
!              Linha Santa Bárbara, s/n, Francisco Beltrão, Paraná, Brazil
!              Zip Code 85601-970
!              
!          (b) Federal University of Paraná - UFPR
!              Curitiba, Paraná, Brazil
!              Caixa postal 19040
!              Zip Code 81531-980
!

!> \brief Defines a calendar class

module mod_class_calendar
   implicit none

   ! Makes everything private, except otherwise stated
   private


   !> \brief A calendar class
   type, public :: class_calendar

      private
      character(10) :: date !< Current date
      character(10) :: time !< Current time

   contains

      procedure, public, pass :: get_date !< Returns current date
      procedure, public, pass :: get_time !< Returns current time

   end type



contains


   !> \brief Returns current date
   function get_date(this) result(date)
      implicit none
      class(class_calendar)     :: this !< A reference to this object
      character(:), allocatable :: date !< Current date

      call get_date_time(this%date, this%time)

      date = this%date

   end function



   !> \brief Returns current time
   function get_time(this) result(time)
      implicit none
      class(class_calendar)     :: this !< A reference to this object
      character(:), allocatable :: time !< Current time

      call get_date_time(this%date, this%time)

      time = this%time

   end function



   !> \brief Obtains system date and time
   subroutine get_date_time(date, time)
      implicit none
      character(10), intent(out) :: date  !< system date
      character(10), intent(out) :: time  !< system time

      character(2)  :: aux1      ! Auxiliary variable
      character(2)  :: aux2      ! Auxiliary variable
      character(4)  :: aux3      ! Auxiliary variable
      character(20) :: vardate   ! system date
      character(20) :: vartime   ! system time
      character(20) :: varzone   ! system zone
      character(50) :: aux       ! Auxiliary variable
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
