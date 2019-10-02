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

!> \brief Provides an interface for a factory class for creating search strategy objects

module mod_class_abstract_search_strategy_factory

   use mod_class_system_variables
   use mod_class_abstract_search_strategy

   implicit none

   !> \brief Interface for a factory class for creating search strategy objects
   type, abstract, public :: class_abstract_search_strategy_factory

   contains

      procedure(create_interface), deferred, public, pass :: create !< Creates a search strategy object

   end type

   abstract interface

      !> \brief Creates an instance of the search strategy object
      subroutine create_interface(this, sys_var, conf_file_name, searcher)

         import class_abstract_search_strategy_factory
         import class_system_variables
         import class_abstract_search_strategy

         implicit none
         class(class_abstract_search_strategy_factory)               :: this           !< A reference to this object
         class(class_system_variables),                  intent(in)  :: sys_var        !< System's variables
         character(len=*),                               intent(in)  :: conf_file_name !< Configuration file
         class(class_abstract_search_strategy), pointer, intent(out) :: searcher       !< Search strategy object


      end subroutine

   end interface

end module
