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

!> \brief Defines a factory class for creating fitness calculator objects.

module mod_class_fitness_calculator_factory

   use mod_mpi
   use mod_class_system_variables
   use mod_class_ehist
   use mod_class_abstract_fitness_calculator
   use mod_class_external_fitness_calculator

   implicit none


   ! Makes everything private, except otherwise stated
   private


   !> \brief Factory class for creating fitness calculator objects
   type, public :: class_fitness_calculator_factory

   contains

      procedure, public, pass :: create !< Creates a fitness calculator object

   end type


contains


   !> \brief Creates a fitness calculator object
   subroutine create(this, sys_var, ehist, model, fitness_calculator)
      implicit none
      class(class_fitness_calculator_factory)                        :: this               !< A reference to this object
      class(class_system_variables),                     intent(in)  :: sys_var            !< System's variables
      class(class_ehist),                                intent(in)  :: ehist              !< Evolution history
      character(len=*),                                  intent(in)  :: model              !< Model
      class(class_abstract_fitness_calculator), pointer, intent(out) :: fitness_calculator !< Fitness calculator


      ! Allocating object

      if ( .not. associated(fitness_calculator) ) then

         if ( trim(model) == "EXTERNAL_CALCULATOR" ) then

            allocate(class_external_fitness_calculator::fitness_calculator)

         else

            call sys_var%logger%println("Unkown model. Stopping.")

            call mod_mpi_finalize()

         end if


         ! Initializing the object
         select type (fitness_calculator)

            type is ( class_external_fitness_calculator )

               call fitness_calculator%init(sys_var, ehist)

         end select

      else

         call sys_var%logger%println("Pointer already associated. Stopping.")

         call mod_mpi_finalize()

      end if


   end subroutine


end module
