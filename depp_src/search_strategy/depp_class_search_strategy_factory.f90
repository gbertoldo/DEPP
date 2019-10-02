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

!> \brief Provides a factory class for creating search strategy objects

module mod_class_search_strategy_factory

   use mod_string
   use mod_class_ifile
   use mod_class_system_variables
   use mod_class_abstract_search_strategy_factory
   use mod_class_abstract_search_strategy
   use mod_class_DE_RAND_1
   use mod_class_DE_RSM
   use mod_class_RSM_search_strategy
   use mod_mpi

   implicit none

   !> \brief Factory class for creating search strategy objects
   type, public, extends(class_abstract_search_strategy_factory) :: class_search_strategy_factory

   contains

      procedure, public, pass :: create !< Creates an instance of the search strategy object

   end type

contains

   !> \brief Creates an instance of the search strategy object
   subroutine create(this, sys_var, conf_file_name, searcher)
      implicit none
      class(class_search_strategy_factory)                        :: this           !< A reference to this object
      class(class_system_variables),                  intent(in)  :: sys_var        !< System's variables
      character(len=*),                               intent(in)  :: conf_file_name !< Configuration file
      class(class_abstract_search_strategy), pointer, intent(out) :: searcher       !< Search strategy object

      ! Inner variables
      type(class_ifile)   :: ifile
      character(str_size) :: CID

      ! Reading configuration file

      call ifile%init(filename=conf_file_name, field_separator='&')

      call ifile%load()

      call ifile%get_value(CID,"CID")

      CID = trim(adjustl(CID))

      if ( .not. associated(searcher) ) then

         ! Allocating the selected model

         if ( trim(CID) == "RSM" ) then

            allocate(class_RSM_search_strategy::searcher)

         else if ( trim(CID) == "DE-RSM" ) then

            allocate(class_DE_RSM::searcher)

         else if ( trim(CID) == "DE/RAND/1" ) then

            allocate(class_DE_RAND_1::searcher)

         else

            call sys_var%logger%println("class_search_strategy_factory: Unknown search strategy model. Stopping.")

            call mod_mpi_finalize()

         end if


         ! Initializing the object
         select type (searcher)

            type is ( class_RSM_search_strategy )

               call searcher%init(sys_var, conf_file_name)

            type is ( class_DE_RSM )

               call searcher%init(sys_var, conf_file_name, this)

            type is ( class_DE_RAND_1 )

               call searcher%init(sys_var, conf_file_name)

         end select

      else

         call sys_var%logger%println("class_search_strategy_factory: Pointer already associated. Stopping.")

         call mod_mpi_finalize()

      end if

   end subroutine

end module
