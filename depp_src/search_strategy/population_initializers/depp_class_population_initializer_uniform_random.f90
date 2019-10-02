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

!> \brief Provides a population initializer that generates trial individuals
!! selected randomly, with uniform a distribution, from the search domain.

module mod_class_population_initializer_uniform_random

   use mod_class_abstract_population_initializer
   use mod_class_ehist
   use mod_search_tools

   implicit none

   ! Makes everything private, except otherwise stated
   private

   !> \brief Population initializer class that generates trial individuals
   !! selected randomly, with uniform a distribution, from the search domain.
   type, extends(class_abstract_population_initializer), public :: class_population_initializer_uniform_random

   contains

      procedure, pass, public :: get_trial !< Generates a trial individual

   end type

contains

   !> \brief Gets a trial individual
   subroutine get_trial(this, ind, ehist, x, es)
      implicit none
      class(class_population_initializer_uniform_random) :: this  !< A reference to this object
      integer,                               intent(in)  :: ind   !< Number of the individual of the population
      class(class_ehist),                    intent(in)  :: ehist !< Evolution history
      real(8), dimension(:),                 intent(out) :: x     !< Trial individual
      integer, optional,                     intent(out) :: es    !< Exit status

      if (present(es)) es = 0

      call get_random_individual(ehist%nu, ehist%xmin, ehist%xmax, x)

   end subroutine

end module
