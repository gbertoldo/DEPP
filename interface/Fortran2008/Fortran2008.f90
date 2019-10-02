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

!> \mainpage Functions
!!
!! Fortran2008 is an external program to exemplifies the application of DEPP
!!
!! \authors
!!
!!          Jonas Joacir Radtke
!!
!!                 E-mail: jonas.radtke@gmail.com
!!             Curriculum: http://lattes.cnpq.br/7202527344656915
!!                    URL: http://paginapessoal.utfpr.edu.br/jonas
!!
!!
!!          Guilherme Bertoldo
!!
!!                 E-mail: glbertoldo@gmail.com
!!             Curriculum: http://lattes.cnpq.br/0176403556779673
!!
!!
!! \par Institution
!!          Federal University of Technology - Paraná - UTFPR
!!
!!
!! \date Jan, 2018.
!!
!! \version 2.0
!!
!! \par GitHub repository
!!          https://github.com/gbertoldo/DEPP
!!
program functions

   use depp_interface

   implicit none
   integer                            :: estatus !< Exit status (0 = success, 1 = failure, 2 = generate another individual)
   real(8)                            :: fit     !< fitness
   real(8), dimension(:), allocatable :: x       !< parameters
   

   ! Exit status
   estatus = 0
       
   
   ! Reads the parameters from DEPP
   call depp_get_parameters(x)


   ! Calculates fitness function
   fit = function1(size(x), x) ! Sphere function (Coley, page 38, 1999)


   ! Saves the fitness function to a file
   call depp_save_fitness(fit, estatus)

   
contains

   !> \brief Sphere function (Coley, page 38, 1999)
   real(8) function function1(nu, x)
      implicit none
      integer, intent(in) :: nu     !< number of unknowns
      real(8), intent(in) :: x(nu)  !< parameters (-5.12 < x(i) < 5.12)
      
      integer :: i
      
      function1 = 79.d0
      do i = 1, nu
         function1 = function1 - x(i)**2
      end do
      
   end function function1

end program functions

