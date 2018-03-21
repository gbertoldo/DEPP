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

