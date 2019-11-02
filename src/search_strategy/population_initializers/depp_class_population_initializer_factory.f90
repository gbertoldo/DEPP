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

!> \brief Provides a factory for population initializers classes

module mod_class_population_initializer_factory

   use mod_mpi
   use mod_string
   use mod_class_ifile
   use mod_class_ehist
   use mod_class_system_variables
   use mod_class_abstract_population_initializer
   use mod_class_population_initializer_uniform_random

   implicit none

   ! Makes everything private, except otherwise stated
   private

   !> \brief A factory for population initializers classes
   type, public :: class_population_initializer_factory

   contains

      procedure, pass, public :: create !< Creates a population initializer object

   end type

contains

   !> \brief Creates a population initializer object
   subroutine create(this, sys_var, conf_file_name, population_initializer)
      implicit none
      class(class_population_initializer_factory)                        :: this                   !< A reference to this object
      class(class_system_variables),                         intent(in)  :: sys_var                !< System's variables
      character(len=*),                                      intent(in)  :: conf_file_name         !< Configuration file
      class(class_abstract_population_initializer), pointer, intent(out) :: population_initializer !< Population initializer object

      ! Inner variables
      type(class_ifile)       :: ifile
      character(len=str_size) :: model

      ! Reading population initializer model
      call ifile%init(filename=sys_var%absparfile, field_separator='&')

      call ifile%load()

      call ifile%get_value(model,"pop_initializer_model")


      ! Allocating object

      if ( .not. associated(population_initializer) ) then

         if ( trim(model) == "uniform_random" ) then

            allocate(class_population_initializer_uniform_random::population_initializer)

         else

            call sys_var%logger%println("class_population_initializer_factory: Unkown model. Stopping.")

            call mod_mpi_finalize()

         end if


         ! Initializing the object
         select type (population_initializer)

            type is ( class_population_initializer_uniform_random )

         end select

      else

         call sys_var%logger%println("class_population_initializer_factory: Pointer already associated. Stopping.")

         call mod_mpi_finalize()

      end if

   end subroutine


end module
