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
!
!> \mainpage DEPP
!!
!! \par DIFFERENTIAL EVOLUTION PARALLEL PROGRAM 2.0
!!
!! \authors
!!
!!          Jonas Joacir Radtke (a)
!!
!!                 E-mail: jonas.radtke@gmail.com
!!             Curriculum: http://lattes.cnpq.br/7202527344656915
!!                    URL: http://paginapessoal.utfpr.edu.br/jonas
!!
!!
!!          Guilherme Bertoldo (a)
!!
!!                 E-mail: glbertoldo@gmail.com
!!             Curriculum: http://lattes.cnpq.br/0176403556779673
!!
!!
!!          Carlos Henrique Marchi (b)
!!
!!                 E-mail: chmcfd@gmail.com
!!             Curriculum: http://lattes.cnpq.br/8251643344377056
!!
!!
!! \par Institution
!!          (a) Federal University of Technology - Paraná - UTFPR
!!          (b) Federal University of Paraná - UFPR
!!
!! \date Jun, 2019.
!!
!! \version 2.0
!!
!! \par GitHub repository
!!          https://github.com/gbertoldo/DEPP
!!
!! \section Introduction
!!
!! This is the documentation of the program DEPP, which implements the Differential
!! Evolution (DE) algorithm for non-linear optimization within a rectangular domain.
!! This algorithm also contains a hybridization with the Response Surface Methodology
!! (RSM) that may accelerate the convergence. The program deals only with maximization.
!! Minimization can be taken into account by multiplying the fitness function by -1.
!! The code implementation is MPI-FORTRAN.

!> \brief Main program
program depp

   use mod_class_optimizer
   use mod_mpi

   implicit none

   type(class_optimizer) :: optimizer
   
   call optimizer%init()

   if ( mpio%master ) call GPLMessage()

   call optimizer%run()
   
contains 

   !> \brief Prints GPL Message
   subroutine GPLMessage()
      implicit none
      
      write(*,*)
      write(*,*) "DEPP  Copyright (C) 2019 - Differential Evolution Parallel Program"
      write(*,*)
      write(*,*)
      write(*,*) "This program comes with ABSOLUTELY NO WARRANTY. This is free software,"
      write(*,*) "and you are welcome to redistribute it under certain conditions."
      write(*,*)
      write(*,*) "For details, see https://github.com/gbertoldo/DEPP/blob/master/LICENSE"
      write(*,*)
   
   end subroutine

end program depp
