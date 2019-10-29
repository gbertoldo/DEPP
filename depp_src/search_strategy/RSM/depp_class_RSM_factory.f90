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

!> \brief Defines a factory for creation of RSM objects

module mod_class_RSM_factory

   use mod_mpi
   use mod_class_system_variables
   use mod_class_abstract_RSM
   use mod_class_RSM_Quadratic_Model
   use mod_class_RSM_Incomplete_Quadratic_Model

   implicit none

   ! Makes everything private, except otherwise stated
   private

   !> \brief Defines a factory for creation of RSM objects
   type, public :: class_RSM_factory

   contains

      procedure, pass, public :: create !< Creates the RSM object

   end type

contains


   !> \brief Creates the RSM object
   subroutine create(this, sys_var, option, obj)
      implicit none
      class(class_RSM_factory)                        :: this    !< A reference to this object
      class(class_system_variables),      intent(in)  :: sys_var !< System's variables
      character(len=*),                   intent(in)  :: option  !< RSM model
      class(class_abstract_RSM), pointer, intent(out) :: obj     !< RSM object

      if ( trim(adjustl(option)) == "quadratic" ) then

         allocate(class_RSM_Quadratic_Model::obj)

      else if ( trim(adjustl(option)) == "incomplete_quadratic" ) then

         allocate(class_RSM_Incomplete_Quadratic_Model::obj)

      else

         call sys_var%logger%println("class_RSM_factory: Unknown response surface model. Stopping...")

         call mod_mpi_finalize()

      end if

   end subroutine


end module

