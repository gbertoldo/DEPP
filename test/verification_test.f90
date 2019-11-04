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

program main

   use mod_class_ifile
   use mod_string

   implicit none
  
   ! Parameters
   integer, parameter :: ntest =    4 ! Number of test functions
   integer,           dimension(ntest), parameter :: nf    = (/     1,       5,        8,      9/) ! Test function ID
   real(8),           dimension(ntest), parameter :: bound = (/5.12d0,   1.5d0, 65.536d0,  2.0d0/) ! Test function's bounds [-bound, bound]
   real(8),           dimension(ntest), parameter :: xa    = (/ 0.0d0,   0.0d0,    0.0d0,  1.0d0/) ! Analytical solution
   character(len=19), dimension(ntest), parameter :: fname = (/"sphere             " &
                                                             , "failure susceptible" &
                                                             , "ellipsoid          " &
                                                             , "Rosenbrock         "/)
   
   ! Variables
   real(8)            :: ptol   ! Tolerance of p-measure
   character(len=100) :: ifname ! File name of DEPP input
   
   
   ! Files 
   type(class_ifile) :: ifile1
   type(class_ifile) :: ifile2
   
      
   ! Dummy variables
   integer :: i
   
   ! Reading file name of DEPP input
   if ( COMMAND_ARGUMENT_COUNT() /= 1 ) then
      write(*,*) "Error: file name of DEPP input was expected. Stopping..."
      stop
   else
      CALL GET_COMMAND_ARGUMENT(1, ifname)
   end if
   
   ! Loading input parameters from templates      
   call ifile1%init(filename=ifname,                field_separator= '&')
   call ifile2%init(filename="input_function.txt",  field_separator= '&')
   call ifile1%load()
   call ifile2%load()
   
   call ifile1%get_value(ptol,"ptol")
   

   ! For each test function...
   do i = 1, ntest
   
      ! Replace test function ID number in "input_function.txt"
      call ifile2%set_value(to_string(nf(i)),"nf")
      
      ! Save "input_function.txt"
      call save_test_function_parameters()
      
      ! Replace DEPP's parameters
      call ifile1%set_value(to_string(-bound(i)),              "lower_bound")
      call ifile1%set_value(to_string( bound(i)),              "upper_bound")
      call ifile1%set_value(to_string(     ptol),                     "ptol") 
      call ifile1%set_value(     "dimensionless",                      "kpm") 
      call ifile1%set_value(         "p_measure", "composite_stop_condition") 
                
      ! Save DEPP's parameters
      call save_depp_parameters()
      
      ! Run DEPP
      write(*,*) 
      write(*,*) "Running '" // trim(fname(i)) // "' test function... "
      call EXECUTE_COMMAND_LINE("mpirun -np 4 depp.x depp_par.txt > /dev/null")
      
      ! Check solution
      call check_solution(i)
   
   end do
   
   
   
contains

   !> \brief Save input parameters of DEPP to file
   subroutine save_depp_parameters()
      implicit none
      
      open(10, file="depp_par.txt")
      
      call ifile1%print_read_data(10,"(A50 '  &  ' A30 '  &  ' A20)")
      
      close(10)

   end subroutine
   
   !> \brief Save input parameters of test function to file
   subroutine save_test_function_parameters()
      implicit none
      
      open(10, file="input_function.txt")
      
      call ifile2%print_read_data(10,"(A50 '  &  ' A30 '  &  ' A20)")
      
      close(10)

   end subroutine
   
   !> \brief Compare numerical and analytical solution
   subroutine check_solution(i)
      implicit none
      integer, intent(in) :: i
      
      ! Auxiliary variables
      integer :: nu      
      integer :: j
      integer :: IO
      real(8) :: error
      character(len=10)   :: caux
      character(len=100)  :: sname
      character(len=1000) :: line
                                                             
      real(8), dimension(:), allocatable :: x   ! Numerical solution  

      ! Reading simulation ID
      call ifile1%get_value(sname,"sname")
     
      ! Reading dimension
      call ifile1%get_value(nu,"nu")
      
      ! Allocating solution vector
      allocate( x(nu) )
      
      ! Reading solution from file
      open(10, file="./depp_output/" // trim(sname) // "-logfile.txt")
      
      do j = 1, nu
      
         write(caux,"(A,I2,A)") "x(", j, ")"
         
         rewind(10)
         
         do
            read(10,"(A)",IOStat=IO) line 
            if (IO/=0) then
               write(*,*) "Unnable to read numerical solution from file. Stopping..."
               stop
            end if
            if ( index(trim(line), trim(caux)) > 0 ) then
               read(line, *) x(j)
               exit
            end if
         end do
               
      end do
      
      close(10)
      
      error = sqrt(dot_product(abs(x-xa(i)),abs(x-xa(i))))
      
      write(*,"(A,20(1X,ES14.7))") "Analytical solution: ", x-x+xa(i)
      write(*,"(A,20(1X,ES14.7))") "Numerical  solution: ", x
      write(*,"(A,20(1X,ES14.7))") "              Error: ", error
      write(*,"(A,20(1X,ES14.7))") "          Tolerance: ", ptol
      if ( error <= ptol ) then
         write(*,"(A)")            "             Status:   SUCCESS!"
      else
         write(*,"(A)")            "             Status:   FAILURE!"
      end if
      deallocate( x )

   end subroutine
     
   
end program
