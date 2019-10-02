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

!> \brief Defines the search strategy DE/rand/1

module mod_class_DE_RAND_1

   use mod_random_generator
   use mod_class_abstract_search_strategy
   use mod_class_ehist
   use mod_class_ifile
   use mod_class_system_variables
   use mod_mpi
   use mod_string
   use mod_search_tools

   implicit none

   ! Makes everything private, except otherwise stated
   private

   !> \brief Class defining DE/rand/1 search strategy
   type, public, extends(class_abstract_search_strategy) :: class_DE_RAND_1

      private

      integer :: np  !< Population size
      real(8) :: dif !< Differential parameter
      real(8) :: crs !< Crossing over parameter

   contains

      procedure, public, pass :: init      !< Constructor
      procedure, public, pass :: get_trial !< Gets a trial individual
      procedure, public, pass :: feed_back !< Process the feedback from fitness calculator
      procedure, public, pass :: data_size !< Gives the size of the shared data vector
      procedure, public, pass :: send      !< Send data to other threads
      procedure, public, pass :: recv      !< Receive data from other threads
      procedure, public, pass :: update    !< Perform update calculations after parallel computation cycle

   end type

contains

   !> \brief Constructor
   subroutine init(this, sys_var, conf_file_name)
      implicit none
      class(class_DE_RAND_1)                    :: this           !< A reference to this object
      class(class_system_variables), intent(in) :: sys_var        !< System's variables
      character(len=*),              intent(in) :: conf_file_name !< Configuration file

      ! Inner variables
      type(class_ifile)   :: ifile1
      type(class_ifile)   :: ifile2
      character(str_size) :: CID

      ! Getting parameters
      call ifile1%init(filename=sys_var%absparfile, field_separator='&')
      call ifile2%init(filename=conf_file_name,     field_separator='&')

      call ifile1%load()
      call ifile2%load()

      call ifile1%get_value(this%np, "np")
      call ifile2%get_value(    CID,"CID")

      if (trim(CID)/="DE/RAND/1") then

         call sys_var%logger%println("class_DE_RAND_1: unexpected CID. Stopping.")

         call mod_mpi_finalize()

      end if

      call ifile2%get_value(this%dif,"dif")
      call ifile2%get_value(this%crs,"crs")

   end subroutine


   !> \brief Generates a trial individual
   subroutine get_trial(this, ind, ehist, x, es)
      implicit none
      class(class_DE_RAND_1)                :: this  !< A reference to this object
      integer,                  intent(in)  :: ind   !< Number of the individual of the population
      class(class_ehist),       intent(in)  :: ehist !< Evolution history
      real(8), dimension(:),    intent(out) :: x     !< Trial individual
      integer, optional,        intent(out) :: es    !< Exit status


      ! Inner variables
      integer :: nu
      integer :: np
      integer :: r(3) ! indexes of selected individuals


      ! Setting exit status
      if (present(es)) es = 0


      ! Detecting nu and np
      nu = size(x)
      np = size(ehist%pop,dim=1)

      do

         ! Choosing three individuals from the population
         call select_individuals(np, ind, r)

         x = ehist%pop(r(3),:) + this%dif*(ehist%pop(r(1),:) - ehist%pop(r(2),:))

         call crossing_over(ind, nu, np, this%crs, ehist%pop, x)

         if ( .not. is_X_out_of_range(nu, ehist%xmin, ehist%xmax, x) ) exit

      end do

   end subroutine

   !> \brief Process the feedback from fitness calculator
   subroutine feed_back(this, ind, ehist, fit, ecode)
      implicit none
      class(class_DE_RAND_1)                :: this    !< A reference to this object
      integer,                  intent(in)  :: ind     !< Number of the individual of the population
      class(class_ehist),       intent(in)  :: ehist   !< Evolution history
      real(8),                  intent(in)  :: fit     !< Fitness of the trial individual
      integer,                  intent(in)  :: ecode   !< Error code

   end subroutine


  !> \brief Gives the size of the shared data vector
  integer function data_size(this)
     implicit none
     class(class_DE_RAND_1) :: this !< A reference to this object

     data_size = this%np

  end function


   !> \brief Send data to other threads
   subroutine send(this, i, to_thread)
      implicit none
      class(class_DE_RAND_1)                :: this      !< A reference to this object
      integer,                   intent(in) :: i         !< Index of the shared data
      integer,                   intent(in) :: to_thread !< Receiver thread

   end subroutine


   !> \brief Receive data from other threads
   subroutine recv(this, i, from_thread)
      implicit none
      class(class_DE_RAND_1)                :: this        !< A reference to this object
      integer,                   intent(in) :: i           !< Index of the shared data
      integer,                   intent(in) :: from_thread !< Sender thread

   end subroutine


   !> \brief Perform update calculations after parallel computation cycle
   subroutine update(this)
      implicit none
      class(class_DE_RAND_1) :: this !< A reference to this object

   end subroutine

end module
