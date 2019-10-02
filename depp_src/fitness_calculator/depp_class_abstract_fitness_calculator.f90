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

!> \brief Defines an interface for fitness function calculation

module mod_class_abstract_fitness_calculator

   use mod_class_ehist

   implicit none


   ! Makes everything private, except otherwise stated
   private


   !> \brief Defines an interface for fitness function calculation
   type, abstract, public :: class_abstract_fitness_calculator

   contains

      procedure(get_fitness_interface),     deferred, public,  pass :: get_fitness      !< Calculates the fitness of a trial individual
      procedure(statistics_info_interface), deferred, public,  pass :: statistics_info  !< Returns a string with fitness calculator statistics information
      procedure(save_failure_interface),    deferred, private, pass :: save_failure     !< Register failures in calculations of fitness
      procedure,                                      public,  pass :: data_size        !< Size of the vector to be shared among threads
      procedure,                                      public,  pass :: send             !< Sends information from this threat
      procedure,                                      public,  pass :: recv             !< Receives information from other threads
      procedure,                                      public,  pass :: update           !< Updates class after a parallel computation cycle

   end type


   abstract interface


      subroutine get_fitness_interface(this, ehist, i, x, fit, ecode)
         import class_abstract_fitness_calculator
         import class_ehist
         implicit none
         class(class_abstract_fitness_calculator) :: this !< A reference to this object
         class(class_ehist),          intent(in)  :: ehist
         integer,                     intent(in)  :: i
         real(8), dimension(:),       intent(in)  :: x
         real(8),                     intent(out) :: fit
         integer,                     intent(out) :: ecode

      end subroutine


      function statistics_info_interface(this) result(info)
         import class_abstract_fitness_calculator
         implicit none
         class(class_abstract_fitness_calculator) :: this !< A reference to this object
         character(len=:), allocatable            :: info

      end function


      subroutine save_failure_interface(this, ehist, i, x, fit, ecode)
         import class_abstract_fitness_calculator
         import class_ehist
         implicit none
         class(class_abstract_fitness_calculator) :: this !< A reference to this object
         class(class_ehist),           intent(in) :: ehist
         integer,                      intent(in) :: i
         real(8), dimension(:),        intent(in) :: x
         real(8),                      intent(in) :: fit
         integer,                      intent(in) :: ecode

      end subroutine


   end interface


contains

      !> \brief Size of the vector to be shared among threads
      integer function data_size(this)
         implicit none
         class(class_abstract_fitness_calculator) :: this !< A reference to this object

         ! By default, no data is shared among threads
         data_size = 0

      end function

      !> \brief Sends information from this threat
      subroutine send(this, i, to_thread)
         implicit none
         class(class_abstract_fitness_calculator) :: this      !< A reference to this object
         integer,                      intent(in) :: i         !< Index of the shared vector
         integer,                      intent(in) :: to_thread !< Receiver thread

      end subroutine


      !> \brief Receives information from other threads
      subroutine recv(this, i, from_thread)
         implicit none
         class(class_abstract_fitness_calculator) :: this        !< A reference to this object
         integer,                      intent(in) :: i           !< Index of the shared vector
         integer,                      intent(in) :: from_thread !< Sender thread

      end subroutine


      !> \brief Updates class after a parallel computation cycle
      subroutine update(this)
         implicit none
         class(class_abstract_fitness_calculator) :: this !< A reference to this object

      end subroutine


end module
