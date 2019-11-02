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

!> \brief Defines an abstract class for creating stop condition objects

module mod_class_abstract_stop_condition

   use mod_class_ehist

   implicit none

   ! Makes everything private, except otherwise stated
   private

   !> \brief Public class defining an interface for creating stop condition objects
   type, abstract, public :: class_abstract_stop_condition

   contains

      procedure(compute_stop_condition_interface),      pass, deferred, public :: compute_stop_condition      !< Computes stop condition
      procedure(is_stop_condition_satisfied_interface), pass, deferred, public :: is_stop_condition_satisfied !< Checks if stop condition is satisfied
      procedure(convergence_info_interface)           , pass, deferred, public :: convergence_info            !< Returns a string containing convergence information
      procedure(final_convergence_info_interface)     , pass, deferred, public :: final_convergence_info      !< Returns a string containing final convergence information

   end type

   abstract interface

      !> \brief Computes stop condition
      subroutine compute_stop_condition_interface(this, ehist)
         import class_abstract_stop_condition
         import class_ehist
         implicit none
         class(class_abstract_stop_condition)  :: this !< A reference to this object
         class(class_ehist),       intent(in)  :: ehist

      end subroutine

      !> \brief Checks if stop condition is satisfied
      logical function is_stop_condition_satisfied_interface(this)
         import class_abstract_stop_condition
         implicit none
         class(class_abstract_stop_condition)  :: this !< A reference to this object

      end function

      !> \brief Returns a string containing convergence information
      function convergence_info_interface(this) result(str)
         import class_abstract_stop_condition
         implicit none
         class(class_abstract_stop_condition) :: this !< A reference to this object
         character(len=:), allocatable        :: str

      end function

      !> \brief Returns a string containing final convergence information
      function final_convergence_info_interface(this) result(str)
         import class_abstract_stop_condition
         implicit none
         class(class_abstract_stop_condition) :: this !< A reference to this object
         character(len=:), allocatable        :: str

      end function

   end interface

end module
