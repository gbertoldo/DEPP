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

!> \brief Provides a simple factory subroutine for creating stop condition objects

module mod_stop_condition_factory

   use mod_class_system_variables
   use mod_class_abstract_stop_condition
   use mod_class_max_generation_stop_condition
   use mod_class_p_measure_stop_condition
   use mod_class_no_improvement_stop_condition
   use mod_mpi

   implicit none

contains

   !> \brief Creates an instance of the stop condition object
   subroutine create_stop_condition_object(sys_var, model, stopper)
      implicit none
      class(class_system_variables),                      intent(in) :: sys_var !< System's variables
      character(len=*),                                   intent(in) :: model   !< Model
      class(class_abstract_stop_condition),     pointer, intent(out) :: stopper !< Stop condition object


      if ( .not. associated(stopper) ) then

         ! Reading configuration file


         ! Allocating the selected model

         if ( trim(model) == "max_generations" ) then

            allocate(class_max_generation_stop_condition::stopper)

         else if ( trim(model) == "no_improvement" ) then

            allocate(class_no_improvement_stop_condition::stopper)

         else if ( trim(model) == "p_measure" ) then

            allocate(class_p_measure_stop_condition::stopper)

         else

            call sys_var%logger%println("Unknown stop condition model: " // trim(model) // ". Stopping.")

            call mod_mpi_finalize()

         end if


         ! Initializing the object
         select type (stopper)

            type is ( class_max_generation_stop_condition )

               call stopper%init(sys_var)

            type is ( class_no_improvement_stop_condition )

               call stopper%init(sys_var)

            type is ( class_p_measure_stop_condition )

               call stopper%init(sys_var)

         end select

      else

         call sys_var%logger%println("Variable already associated. Stopping.")

         call mod_mpi_finalize()

      end if

   end subroutine

end module
