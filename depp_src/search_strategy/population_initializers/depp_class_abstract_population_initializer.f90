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

!> \brief Provides an interface for population initialization

module mod_class_abstract_population_initializer

   use mod_class_ehist

   implicit none

   ! Makes everything private, except otherwise stated
   private

   !> \brief Abstract class for population initialization
   type, abstract, public :: class_abstract_population_initializer

   contains

      procedure(get_trial_interface), deferred, pass, public :: get_trial !< Generates a trial individual

   end type

   abstract interface

      !> \brief Gets a trial individual
      subroutine get_trial_interface(this, ind, ehist, x, es)
         import class_abstract_population_initializer
         import class_ehist
         implicit none
         class(class_abstract_population_initializer) :: this  !< A reference to this object
         integer,                         intent(in)  :: ind   !< Number of the individual of the population
         class(class_ehist),              intent(in)  :: ehist !< Evolution history
         real(8), dimension(:),           intent(out) :: x     !< Trial individual
         integer, optional,               intent(out) :: es    !< Exit status

      end subroutine

   end interface

end module
