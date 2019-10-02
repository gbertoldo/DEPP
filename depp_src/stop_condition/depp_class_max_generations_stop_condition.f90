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

!> \brief Defines a stop condition class which stops if the number of generations are greater
!! or equal to a specified value.

module mod_class_max_generation_stop_condition

   use mod_class_abstract_stop_condition
   use mod_class_ehist
   use mod_class_system_variables
   use mod_class_ifile
   use mod_string

   implicit none


   ! Makes everything private, except otherwise stated
   private

   !> \brief Class for creating a maximum number of generation based stop condition object
   type, public, extends(class_abstract_stop_condition) :: class_max_generation_stop_condition

      private
      integer :: g          !< Current generation
      integer :: ng         !< Maximum number of generations allowed
      logical :: stopflag   !< Stop flag
      integer :: verbosity  !< Verbosity level for log

   contains

      procedure, public, pass :: init                        !< Constructor
      procedure, public, pass :: compute_stop_condition      !< Computes stop condition
      procedure, public, pass :: is_stop_condition_satisfied !< Checks if stop condition is satisfied
      procedure, public, pass :: convergence_info            !< Returns a string containing convergence information
      procedure, public, pass :: final_convergence_info      !< Returns a string containing final convergence information

   end type

contains


   !> \brief Constructor
   subroutine init(this, sys_var)
      implicit none
      class(class_max_generation_stop_condition) :: this    !< A reference to this object
      class(class_system_variables),  intent(in) :: sys_var !< System's variables

      ! Inner variables
      type(class_ifile) :: ifile


      ! Reading configuration file
      call ifile%init(filename=trim(sys_var%absparfile), field_separator="&")
      call ifile%load()
      call ifile%get_value(       this%ng,       "ng")
      call ifile%get_value(this%verbosity,"verbosity")

      this%stopflag = .false.

   end subroutine


   !> \brief Computes the stop condition
   subroutine compute_stop_condition(this, ehist)
      implicit none
      class(class_max_generation_stop_condition) :: this  !< A reference to this object
      class(class_ehist),             intent(in) :: ehist !< Evolution history

      this%g = ehist%g

      if (this%g >= this%ng ) this%stopflag = .true.

   end subroutine


   !> \brief Checks if the stop condition was reached
   logical function is_stop_condition_satisfied(this)
      implicit none
      class(class_max_generation_stop_condition) :: this !< A reference to this object

      is_stop_condition_satisfied = this%stopflag

   end function

   !> \brief Returns the convergence information
   function convergence_info(this) result(str)
      implicit none
      class(class_max_generation_stop_condition) :: this !< A reference to this object
      character(len=:), allocatable              :: str  !< String containing convergence information

      ! Inner variables
      character(len=str_size) caux

      caux = ""

      select case (this%verbosity)
          case (0)

          case (1)

             write(caux,"(A, I7, A, I7, A)") "   --->  Current generation: ", this%g, ". Tolerance: ", this%ng, "."

          case (2)

             write(caux,"(A, I7, A, I7, A)") "   --->  Current generation: ", this%g, ". Tolerance: ", this%ng, "."

          case default

      end select

      str = trim(caux)

   end function


   !> \brief Returns the final convergence information
   function final_convergence_info(this) result(str)
      implicit none
      class(class_max_generation_stop_condition) :: this !< A reference to this object
      character(len=:), allocatable              :: str  !< String containing convergence information

      ! Inner variables
      character(len=str_size) caux

      caux = ""

      write(caux,"(A, I23, A)") trim(caux),  this%g, " : last generation"//char(10)
      write(caux,"(A, I23, A)") trim(caux), this%ng, " : maximum number of generations"

      str = trim(caux)

   end function

end module
