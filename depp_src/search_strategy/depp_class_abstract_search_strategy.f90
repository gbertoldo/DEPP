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

!> \brief Defines an abstract class for creating search strategy objects

module mod_class_abstract_search_strategy

   use mod_class_ehist

   implicit none

   ! Makes everything private, except otherwise stated
   private

   !> \brief Public class defining an interface for creating search strategy objects
   type, abstract, public :: class_abstract_search_strategy

   contains

      procedure(get_trial_interface), pass, deferred, public :: get_trial !< Gets a trial individual
      procedure(feed_back_interface), pass, deferred, public :: feed_back !< Process the feedback from fitness calculator
      procedure(data_size_interface), pass, deferred, public :: data_size !< Gives the size of the shared data vector
      procedure(send_interface),      pass, deferred, public :: send      !< Send data to other threads
      procedure(recv_interface),      pass, deferred, public :: recv      !< Receive data from other threads
      procedure(update_interface),    pass, deferred, public :: update    !< Perform update calculations after parallel computation cycle

   end type

   abstract interface

      !> \brief Gets a trial individual
      subroutine get_trial_interface(this, ind, ehist, x, es)
         import class_abstract_search_strategy
         import class_ehist
         implicit none
         class(class_abstract_search_strategy) :: this  !< A reference to this object
         integer,                  intent(in)  :: ind   !< Number of the individual of the population
         class(class_ehist),       intent(in)  :: ehist !< Evolution history
         real(8), dimension(:),    intent(out) :: x     !< Trial individual
         integer, optional,        intent(out) :: es    !< Exit status

      end subroutine

      !> \brief Process the feedback from fitness calculator
      subroutine feed_back_interface(this, ind, ehist, fit, ecode)
         import class_abstract_search_strategy
         import class_ehist
         implicit none
         class(class_abstract_search_strategy) :: this    !< A reference to this object
         integer,                  intent(in)  :: ind     !< Number of the individual of the population
         class(class_ehist),       intent(in)  :: ehist   !< Evolution history
         real(8),                  intent(in)  :: fit     !< Fitness of the trial individual
         integer,                  intent(in)  :: ecode   !< Error code

      end subroutine

      !> \brief Gives the size of the shared data vector
      integer function data_size_interface(this)
      import class_abstract_search_strategy
         implicit none
         class(class_abstract_search_strategy) :: this !< A reference to this object

      end function

      !> \brief Send data to other threads
      subroutine send_interface(this, i, to_thread)
         import class_abstract_search_strategy
         implicit none
         class(class_abstract_search_strategy) :: this      !< A reference to this object
         integer,                   intent(in) :: i         !< Index of the shared data
         integer,                   intent(in) :: to_thread !< Receiver thread

      end subroutine

      !> \brief Receive data from other threads
      subroutine recv_interface(this, i, from_thread)
         import class_abstract_search_strategy
         implicit none
         class(class_abstract_search_strategy) :: this        !< A reference to this object
         integer,                   intent(in) :: i           !< Index of the shared data
         integer,                   intent(in) :: from_thread !< Sender thread

      end subroutine

      !> \brief Perform update calculations after parallel computation cycle
      subroutine update_interface(this)
         import class_abstract_search_strategy
         implicit none
         class(class_abstract_search_strategy) :: this !< A reference to this object

      end subroutine

   end interface

end module
