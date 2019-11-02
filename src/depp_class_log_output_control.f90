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

!> \brief Defines a class for printing log information

module mod_class_log_output_control

   use mod_mpi
   use mod_string

   implicit none


   ! Makes everything private, except otherwise stated
   private


   ! Initialization flag
   logical :: initialized = .false.


   !> \brief Class for printing log information
   type, public :: class_log_output_control

      private
      integer         :: fileid(2) !< File ID's

   contains

      procedure, public, pass :: init    !< Constructor
      procedure, public, pass :: print   !< Print
      procedure, public, pass :: println !< Print with a new line char
      procedure, public, pass :: finish  !< Destructor

   end type


contains


   !> \brief Open files
   subroutine init(this, reload, logfile)
      implicit none
      class(class_log_output_control) :: this    !< A reference to this object
      integer,             intent(in) :: reload  !< Reload? (0=no, 1=yes)
      character(len=*),    intent(in) :: logfile !< File name


      ! Initialize only once
      if ( .not. initialized ) then

         initialized = .true.

         ! Standard output (screen)
         this%fileid(1) = 6

         ! Log file
         this%fileid(2) = 100

         ! Only master
         if (mpio%master) then

            if (reload==0) then

               open(this%fileid(2),file=logfile)

            else

               open(this%fileid(2),file=logfile, position='append')

            end if

         end if

      end if

   end subroutine



   !> \brief Print messages
   subroutine print(this, msg)
      implicit none
      class(class_log_output_control) :: this !< A reference to this object
      character(len=*),    intent(in) :: msg  !< Message to be printed

      ! Inner variables
      integer :: i

      ! Only master
      if (mpio%master) then

         do i = 1, size(this%fileid)

            write(this%fileid(i),"(A)", advance='no') trim(msg)

         end do

      end if

   end subroutine



   !> \brief Print messages with new line char
   subroutine println(this, msg)
      implicit none
      class(class_log_output_control) :: this !< A reference to this object
      character(len=*),    intent(in) :: msg  !< Message to be printed

      call this%print(trim(msg)//ENDL)

   end subroutine



   !> \brief Close files
   subroutine finish(this)
      implicit none
      class(class_log_output_control) :: this !< A reference to this object

      ! Inner variables
      integer :: i

      ! Only master
      if (mpio%master) then

         do i = 2, size(this%fileid)

            close(this%fileid(i))

         end do

      end if

   end subroutine


end module
