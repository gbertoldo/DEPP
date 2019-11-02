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

!> \brief Provides a simple factory subroutine for creating search strategy objects

module mod_search_strategy_factory

   use mod_class_system_variables
   use mod_class_abstract_search_strategy
   use mod_class_DE_RAND_1
   use mod_class_DE_RSM
   use mod_class_ifile

   implicit none

contains

   !> Creates an instance of the individual generator
   recursive subroutine create_search_strategy(sys_var, kh, kss, searcher)
      implicit none
      class(class_system_variables),                  intent(in)  :: sys_var
      INTEGER,                               intent(in)  :: kh
      INTEGER,                               intent(in)  :: kss
      class(class_abstract_search_strategy), pointer, intent(out) :: searcher


      ! Inner variables
      class(class_abstract_search_strategy), pointer :: aux => null()
      type(class_ifile) :: ifile
      integer :: np



      if ( .not. associated(searcher) ) then

         ! Reading configuration file

         ! Allocating the selected model

         if ( kh == 2 ) then

            allocate(class_DE_RSM::searcher)

         else if ( kh == 1 ) then

         else if ( kh == 0 ) then

            if ( kss == 1 ) then

               allocate(class_DE_RAND_1::searcher)

            end if

         else

            write(*,*) "Unknown hybridization model. Stopping."

            stop

         end if


         ! Initializing the object
         select type (searcher)

            type is ( class_DE_RAND_1 )

               call searcher%init()

            type is ( class_DE_RSM )

               call create_search_strategy(sys_var, 0, kss, aux)

               call ifile%init(filename=sys_var%absparfile, field_separator='&')

               call ifile%load()

               call ifile%get_value(np,"np")

               call searcher%init(sys_var, np, aux)

         end select

      else

         write(*,*) "Pointer already associated. Stopping."
         stop

      end if

   end subroutine

end module
