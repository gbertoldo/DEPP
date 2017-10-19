!> \mainpage DEPP
!!
!! \par DIFFERENTIAL EVOLUTION PARALLEL PROGRAM 2.0
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
!!             Curriculum: http://buscatextual.cnpq.br/buscatextual/visualizacv.do?id=H769069
!!
!!
!! \par Institution
!!          Federal University of Technology - Paraná - UTFPR
!!
!!
!! \date Apr, 2016.
!!
!! \version 2.0
!!
!! \par SVN CHECKOUT
!!          http://depp.googlecode.com/svn/trunk/
!!
!! \section Introduction
!!
!! This is the documentation of the program DEPP, which implements the Differential
!! Evolution (DE) algorithm for non-linear optimization within a rectangular domain.
!! This algorithm also contains an hybridization with the Response Surface Methodology
!! (RSM) that may accelerate the convergence. The program deals only with maximization.
!! Minimization can be taken into account by multiplying the fitness function by -1.
!! The code implementation is MPI-FORTRAN 90.

!> \brief Main program
program depp

   use mod_class_optimizer

   implicit none

   type(class_optimizer) :: optimizer

   call optimizer%init()

   call optimizer%run()

end program depp
