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

!> \brief Creates an interface to DEPP optimizer
module depp_interface
   implicit none

   character (len=200),  private :: arqfit = "" !< name of file for the fitness

contains

   !> \brief Reads the parameters from DEPP
   subroutine depp_get_parameters(x)
      implicit none
      real(8), allocatable, intent(out) :: x(:) !< Values of the variables
      
      ! Inner variables
      integer             :: i     ! Dummy index
      integer             :: nu    ! Number of unknowns
      character(len=2000) :: ifile ! Input file

      ! Getting the input file name
      call get_command_argument(1, ifile)
      
      ! Openning input file
      open(10,file=trim(ifile))

      ! Initializing global variables

      arqfit = "" ! name of file for the fitness

      ! Reading the filename where the fitness will be saved

      read(10,*) arqfit

      if ( trim(adjustl(arqfit)) == "" ) then

         write(*,*) "depp_get_parameters: empty output file name. Stopping"

      end if

      ! Reading the number of unknowns
      read(10,*) nu

      ! Allocating memory
      allocate(x(nu))

      ! Reading the unknowns and their names
      do i = 1, nu

         read(10,*) x(i)

      end do
      
      close(10)


   end subroutine depp_get_parameters



   !> \brief Saves the fitness function to a file
   subroutine depp_save_fitness(fitness, estatus)
      implicit none
      real(8), intent(in) :: fitness !< Fitness for a given individual
      integer, intent(in) :: estatus !< Exit status ( 0 = success, 1 = failure, 2 = generate another individual)


      if ( trim(adjustl(arqfit)) /= "" ) then

         open(10, file = trim(adjustl(arqfit)) )

         write(10,"(ES23.16,A)") fitness, " = Fitness" 

         write(10,"(I23,A)") estatus, " = Exit status ( 0 = success, 1 = failure, 2 = generate another individual)"

         close(10)

      end if

   end subroutine depp_save_fitness


end module depp_interface

!> \mainpage Functions
!!
!! Functions is an external program to exemplifies the application of DEPP
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
   integer                            :: nf      !< function number
   integer                            :: estatus !< Exit status (0 = success, 1 = failure, 2 = generate another individual)
   real(8)                            :: fit     !< fitness
   real(8), dimension(:), allocatable :: x       !< parameters
   
   ! Variables used for creating seeds of the random number generator
   integer :: clock     !< clock time for seeds generation
   integer :: seed(97)  !< seeds for random numbers

      
   ! Initializing the seed for the random numbers
   call system_clock(count = clock)
   seed = clock
   call random_seed(put = seed)


   ! Exit status
   estatus = 0
       
   
   ! Reads the parameters from DEPP
   call depp_get_parameters(x)


   ! Choosing the fitness function
   open(10, file = "./input_function.txt")
   read(10,*) nf
   close(10)


   ! Calculating the fitness 
   select case (nf)
      case(1)
         fit = function1(size(x), x) ! Sphere function (Coley, page 38, 1999)
      case(2)
         fit = function2(size(x), x) ! Step function (Zhang and Sanderson, 2009)
      case(3)
         fit = function3(size(x), x) ! Test function 4 (Coley, page 38, 1999)
      case(4)
         fit = function4(size(x), x) ! Test function 5 (Coley, page 38, 1999)
      case(5)
         fit = function5(size(x), x, estatus) 
         ! Test function 5 (this function is used to test the exit status functionality
         ! of the DEPP program)
      case(6)
         fit = function6(size(x), x, estatus)  ! Bump problem (Feoktistov, page 139, 2006)
      case(7)
         fit = function7(size(x), x)  ! Rastrigin's function (Feoktistov, page 163, 2006)
      case(8)
         fit = function8(size(x), x)  ! Rotated Ellipsoid function (Feoktistov, page 164, 2006)
      case(9)
         fit = function9(size(x), x)  ! Rosenbrock's function, (Feoktistov, page 160, 2006) 
      case(10)
         fit = function10(size(x), x) ! Shifted Ackley's function (Vicenzi and Savoia, 2015)
      case(11)
         fit = function11(size(x), x) ! Schwefel 2.22 (Zhang and Sanderson, 2009)
      case(12)
         fit = function12(size(x), x) ! Schwefel 2.21 (Zhang and Sanderson, 2009)
      case(13)
         fit = function13(size(x), x) ! Noisy quartic (Zhang and Sanderson, 2009)
      case(14)
         fit = function14(size(x), x) ! Schwefel 2.26 (Zhang and Sanderson, 2009)
      case(15)
         fit = function15(size(x), x) ! Griewank (Zhang and Sanderson, 2009)
      case default
         write(*,*) " ERROR: invalid function number. "
         stop
   end select


   ! Saves the fitness function to a file
   call depp_save_fitness(fit, estatus)

   
contains

   !============================================================================

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

   !============================================================================

   !> \brief Step function (Zhang and Sanderson, 2009)
   real(8) function function2(nu, x)
      implicit none
      integer, intent(in) :: nu     !< number of unknowns
      real(8), intent(in) :: x(nu)  !< parameters (-100 < x(i) < 100)
      
      integer :: i
       
      function2 = 0.d0
      do i = 1, nu
         function2 = function2 - dble(floor(x(i)+0.5d0))**2
      end do
   
   end function function2

   !============================================================================

   !> \brief Test function F4 (Coley, page 38, 1999)
   real(8) function function3(nu, x)
      implicit none
      integer, intent(in) :: nu     !< number of unknowns
      real(8), intent(in) :: x(nu)  !< parameters (-100 < x(i) < 100)

      if (nu == 2) then
         function3 = 0.5d0 - ((dsin(dsqrt(x(1)**2 + x(2)**2)))**2 &
            - 0.5d0)/(1.d0 + 0.001d0*(x(1)**2 + x(2)**2))**2
      else
         write(*,*) " ERROR: invalid number of unknowns for function 4. "
         stop
      end if
   
   end function function3

   !============================================================================

   !> \brief Test function F5 (Coley, page 38, 1999)
   real(8) function function4(nu, x)
      implicit none
      integer, intent(in) :: nu     !< number of unknowns
      real(8), intent(in) :: x(nu)  !< parameters (-40 < x(i) < 60)

      integer :: i
      real(8) :: a
      
      a = 5.d0
      
      function4 = a
      
      do i = 1, nu
         function4 = function4 - int(x(i) + 0.5d0)**2
      end do
   
   end function function4

   !============================================================================

   !> \brief Test function 5 (this function is used to test the exit status functionality
   !! of the DEPP program)
   real(8) function function5(nu, x, estatus)
      implicit none
      integer, intent(in)  :: nu      !< number of unknowns
      real(8), intent(in)  :: x(nu)   !< parameters (-1.5 < x(i) < 1.5)
      integer, intent(out) :: estatus !< exit status (0 = success, 1 = failure, 2 = generate another individual)

      ! Inner variables

      integer :: i
      
      function5 = 0.d0

      do i = 1, nu

         function5 = function5 - x(i) * x(i)

      end do

      ! Checking status
      if ( function5 >= -1.d0 ) then ! Success

         estatus = 0

      else if ( -1.d0 > function5 .and. function5 > -2.d0 ) then ! Generate another individual

         estatus = 2

      else ! Failure

         estatus = 1

      end if
   
   end function function5

   !============================================================================

   !> \brief Bump problem (Feoktistov, page 139, 2006)
   real(8) function function6(nu, x, estatus)
      implicit none
      integer, intent(in)  :: nu      !< number of unknowns
      real(8), intent(in)  :: x(nu)   !< parameters (0 < x(i) < 10)
      integer, intent(out) :: estatus !< exit status (0 = success, 1 = failure, 2 = generate another individual)

      integer :: i
      real(8) :: sum1
      real(8) :: sum2
      real(8) :: sum3
      real(8) :: mult1
      real(8) :: mult2

      
      sum1 = 0.d0
      sum2 = 0.d0
      sum3 = 0.d0

      mult1 = 2.d0
      mult2 = 1.d0
      
      do i = 1, nu
         sum1 = sum1 + dcos(x(i))**4
         sum2 = sum2 + dble(i)*x(i)**2
         sum3 = sum3 + x(i) 
         
         mult1 = mult1 * dcos(x(i))**2
         mult2 = mult2 * x(i)

      end do
      
      function6 = dabs(sum1 - mult1)/dsqrt(sum2)


      ! Checking the constraints
      
      if ( sum3 < 15.d0*nu/2.d0 .and. 0.75d0 < mult2 ) then ! Constraints satisfied
      
         estatus = 0
         
      else ! Ask for another individual, because the constraints were not satisfied
      
         estatus = 2
         
      end if
   
   end function function6

   !============================================================================

   !> \brief Rastrigin's function (Feoktistov, page 163, 2006)
   real(8) function function7(nu, x)
      implicit none
      integer, intent(in) :: nu     !< number of unknowns
      real(8), intent(in) :: x(nu)  !< parameters (-5.12 < x(i) < 5.12)
      
      integer :: i
      real(8), parameter :: pi = dcos(-1.d0)
       
      function7 = 0.d0
      do i = 1, nu
         function7 = function7 + x(i)**2 - 10.d0*dcos(2.d0*pi*x(i)) + 10.d0
      end do

      function7 = - function7
   
   end function function7


   !============================================================================

   !> \brief Rotated Ellipsoid function (Feoktistov, page 164, 2006)
   real(8) function function8(nu, x)
      implicit none
      integer, intent(in) :: nu     !< number of unknowns
      real(8), intent(in) :: x(nu)  !< parameters (-65.536 < x(i) < 65.536)
      
      integer :: i
      integer :: j
      real(8) :: sum1
      
      function8 = 0.d0
      do i = 1, nu
      
         sum1 = 0.d0
         
         do j = 1, i
            sum1 = sum1 + x(j)
         end do
         
         function8 = function8 - sum1**2
         
      end do
   
   end function function8

   !============================================================================
   
   !> \brief Rosenbrock (Feoktistov, page 160, 2006)
   real(8) function function9(nu, x)
      implicit none
      integer, intent(in) :: nu     !< number of unknowns
      real(8), intent(in) :: x(nu)  !< parameters (-2.048 < x(i) < 2.048)

      integer :: i
      
      function9 = 0.d0
      
      do i = 1, nu-1
      
         function9 = function9 + 100.d0*(x(i)**2 - x(i+1))**2 + (1.d0 - x(i))**2
      
      end do
      
      function9 = -function9
   
   end function function9
   

   !============================================================================

   !> \brief Shifted Ackley's function (Vicenzi and Savoia, 2015)
   real(8) function function10(nu, x)
      implicit none
      integer, intent(in) :: nu     !< number of unknowns
      real(8), intent(in) :: x(nu)  !< parameters (-5.12 < x(i) < 5.12)
      
      integer :: i
      real(8) :: sum1
      real(8) :: sum2
      real(8), parameter :: pi = dcos(-1.d0)
      
      sum1 = 0.d0
      sum2 = 0.d0
      
      do i = 1, nu
         sum1 = sum1 + (x(i)-1.d0)**2
         sum2 = sum2 + dcos(2.d0*pi*(x(i)-1.d0))
      end do
   
      function10 = -20.d0*dexp(-0.2d0*dsqrt(1.d0/dble(nu)*sum1)) &
         - dexp(1.d0/dble(nu)*sum2) + 20.d0 + dexp(1.d0)

      function10 = - function10
   
   end function function10

   !============================================================================

   !> \brief Schwefel 2.22 (Zhang and Sanderson, 2009)
   real(8) function function11(nu, x)
      implicit none
      integer, intent(in) :: nu     !< number of unknowns
      real(8), intent(in) :: x(nu)  !< parameters (-10 < x(i) < 10)
      
      
      integer :: i
      real(8) :: sum1
      real(8) :: mul1

      
      sum1 = sum(abs(x))
      
      mul1 = 1.d0
      
      do i = 1, nu
      
         mul1 = mul1 * abs(x(i))
         
      end do

      
      function11 = sum1 + mul1
      

      function11 = - function11
   
   end function function11

   !============================================================================

   !> \brief Schwefel 2.21 (Zhang and Sanderson, 2009)
   real(8) function function12(nu, x)
      implicit none
      integer, intent(in) :: nu     !< number of unknowns
      real(8), intent(in) :: x(nu)  !< parameters (-100 < x(i) < 100)
      
      function12 = - maxval(abs(x))
   
   end function function12

   !============================================================================
   
   !> \brief Noisy quartic (Zhang and Sanderson, 2009)
   real(8) function function13(nu, x)
      implicit none
      integer, intent(in) :: nu     !< number of unknowns
      real(8), intent(in) :: x(nu)  !< parameters (-1.28 < x(i) < 1.28)
     
      
      integer :: i
      real(8) :: rnd


      call random_number(rnd)

      function13 = rnd
      
      do i = 1, nu
            
         function13 = function13 + x(i)**4 * dble(i) 
         
      end do


      function13 = - function13
   
   end function function13

   !============================================================================


   !> \brief Schwefel 2.26 (Zhang and Sanderson, 2009)
   real(8) function function14(nu, x)
      implicit none
      integer, intent(in) :: nu     !< number of unknowns
      real(8), intent(in) :: x(nu)  !< parameters (-500 < x(i) < 500)
      
      integer :: i
      
      function14 = 0.d0
      
      do i = 1, nu
      
         function14 = function14 - x(i) * sin( sqrt( abs(x(i)) ) )
      
      end do
      
      function14 = function14 + dble(nu) * 418.98288727243369d0

      function14 = - function14
   
   end function function14
   

   !============================================================================
   

   !> \brief Griewank (Zhang and Sanderson, 2009)
   real(8) function function15(nu, x)
      implicit none
      integer, intent(in) :: nu     !< number of unknowns
      real(8), intent(in) :: x(nu)  !< parameters (-600 < x(i) < 600)
            
      integer :: i
      real(8) :: sum1
      real(8) :: mul1

      sum1 = sum( x*x ) / 4000d0
      
      mul1 = 1.d0
      
      do i = 1, nu
      
         mul1 = mul1 * cos(x(i)/sqrt(dble(i)))
      
      end do
      
      function15 = sum1 - mul1 + 1.d0

      function15 = - function15
   
   end function function15

end program functions

