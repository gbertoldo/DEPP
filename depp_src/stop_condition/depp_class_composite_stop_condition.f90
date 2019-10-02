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

!> \brief Defines a composite stop condition class.

module mod_class_composite_stop_condition

   use mod_class_abstract_stop_condition
   use mod_stop_condition_factory
   use mod_class_ehist
   use mod_class_system_variables
   use mod_class_ifile
   use mod_string

   implicit none


   ! Makes everything private, except otherwise stated
   private


   !> \brief A pointer to a stop condition object
   type :: ptr_stop_cond

      class(class_abstract_stop_condition), pointer :: ptr => null()

   end type


   !> \brief Class for creating a composite of stop condition objects
   type, public, extends(class_abstract_stop_condition) :: class_composite_stop_condition

      private
      integer :: nsc       !< Number of stop conditions
      logical :: stopflag  !< Stop flag
      integer :: verbosity !< Verbosity level for log

      type(ptr_stop_cond), allocatable, dimension(:) :: stop_cond_container !< A container of stop conditions objects

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
      class(class_composite_stop_condition)      :: this    !< A reference to this object
      class(class_system_variables),  intent(in) :: sys_var !< System's variables

      ! Inner variables
      type(class_ifile)  :: ifile
      ! Auxiliary variables
      integer :: i
      integer :: IO
      character(len=str_size) :: caux
      character(len=str_size) :: models(100)


      ! Reading configuration file
      call ifile%init(filename=trim(sys_var%absparfile), field_separator="&")
      call ifile%load()
      call ifile%get_value(caux,"composite_stop_condition")
      call ifile%get_value(this%verbosity,"verbosity")

      ! Identifying stop conditions
      models = "null"

      read(caux,*, IOstat=IO) models

      do i = 1, size(models)

         if (trim(models(i))=="null") then

            this%nsc = i-1

            exit

         end if

      end do

      ! Creating stop condition objects

      allocate(this%stop_cond_container(this%nsc))

      do i = 1, this%nsc

         call create_stop_condition_object(sys_var, model=models(i), stopper=this%stop_cond_container(i)%ptr)

      end do

      this%stopflag = .false.

   end subroutine


   !> \brief Computes the stop condition
   subroutine compute_stop_condition(this, ehist)
      implicit none
      class(class_composite_stop_condition)      :: this  !< A reference to this object
      class(class_ehist),             intent(in) :: ehist !< Evolution history

      ! Inner variables
      integer :: i

      do i = 1, this%nsc

         call this%stop_cond_container(i)%ptr%compute_stop_condition(ehist)

      end do

   end subroutine


   !> \brief Checks if the stop condition is satisfied
   logical function is_stop_condition_satisfied(this)
      implicit none
      class(class_composite_stop_condition) :: this !< A reference to this object

      ! Inner variables
      integer :: i

      is_stop_condition_satisfied = .false.

      do i = 1, this%nsc

         if ( this%stop_cond_container(i)%ptr%is_stop_condition_satisfied() ) then

            is_stop_condition_satisfied = .true.

         end if

      end do

   end function


   !> \brief Returns the convergence information
   function convergence_info(this) result(str)
      implicit none
      class(class_composite_stop_condition) :: this !< A reference to this object
      character(len=:), allocatable         :: str  !< String containing convergence information

      ! Inner variables
      integer :: i


      select case (this%verbosity)
          case (0)

            str = ""

          case default

             str = "Convergence info: " // char(10) ! char(10) = new line char

             do i = 1, this%nsc

                str = str // this%stop_cond_container(i)%ptr%convergence_info() // char(10)

             end do

      end select

   end function


   !> \brief Returns the final convergence information
   function final_convergence_info(this) result(str)
      implicit none
      class(class_composite_stop_condition) :: this !< A reference to this object
      character(len=:), allocatable         :: str  !< String containing convergence information

      ! Inner variables
      integer :: i


      str = " ================  CONVERGENCE INFO  ================= " // char(10) // char(10) ! char(10) = new line char

      do i = 1, this%nsc

         str = str // this%stop_cond_container(i)%ptr%final_convergence_info() // char(10)

      end do

   end function

end module
