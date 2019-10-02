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

!> \brief Contains tools to handle strings

module mod_string

   implicit none

   ! Standard string size
   integer,   parameter :: str_size = 1000
   character, parameter :: ENDL = char(10)

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
