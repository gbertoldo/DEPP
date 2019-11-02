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

!> \brief Defines a timer class proper to MPI.

module mod_class_timer

   use mpi

   implicit none


   ! All members are private by default
   private


   !> \brief Class timer
   type, public :: class_timer

      private

      real(8) :: t0 = 0.d0
      real(8) :: t1 = 0.d0

   contains

      procedure, public, pass :: start                   !< Starts timer
      procedure, public, pass :: measure                 !< Measures the elapsed time
      procedure, public, pass :: elapsed_time            !< Returns the elapsed time in seconds
      procedure, public, pass :: formatted_elapsed_time  !< Returns the elapsed time formatted as 00:00:00

   end type

contains


   !> \brief Starts timer
   subroutine start(this, accumulated)
      class(class_timer)            :: this        !< A reference to this object
      real(8), optional, intent(in) :: accumulated !< Accumulated time from another reading

      if ( present(accumulated) ) then

         this%t0 = MPI_Wtime() + accumulated

      else

         this%t0 = MPI_Wtime()

      end if

      this%t1 = this%t0

   end subroutine


   !> \brief Measure time
   subroutine measure(this)
      class(class_timer) :: this !< A reference to this object

      this%t1 = MPI_Wtime()

   end subroutine


   !> \brief Returns the elapsed time in seconds
   real(8) function elapsed_time(this)
      class(class_timer) :: this !< A reference to this object

      elapsed_time = this%t1 - this%t0

   end function


   !> \brief Returns the elapsed time formatted as 00:00:00
   character(len=10) function formatted_elapsed_time(this)
      class(class_timer) :: this !< A reference to this object

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
