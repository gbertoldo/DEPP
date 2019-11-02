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

!> \brief Defines the search strategy DE-RSM

module mod_class_DE_RSM

   use mod_class_abstract_search_strategy_factory
   use mod_class_abstract_search_strategy
   use mod_random_generator
   use mod_class_system_variables
   use mod_mpi
   use mod_class_ehist
   use mod_class_RSM_search_strategy
   use mod_search_tools
   use mod_class_ifile
   use mod_string
   use mod_class_DE_RSM_hybridization_control


   implicit none

   ! Makes everything private, except otherwise stated
   private

   !> \brief DE-RSM search strategy
   type, public, extends(class_abstract_search_strategy) :: class_DE_RSM

      private

      ! These data are calculated separately by each thread and must be exchanged
      ! before the next generation.
      integer, allocatable                           :: rsm_tag(:)   !< Stores the return state of application of DE-RSM
      real(8), allocatable                           :: curnt_fit(:) !< Fitness of the current individuals
      real(8), allocatable                           :: trial_fit(:) !< Fitness of the trial individuals

      ! Hybridization control
      type(class_DE_RSM_hybridization_control)       :: hybrid_control !< Controls DE-RSM hybridization

      ! Search strategies
      class(class_abstract_search_strategy), pointer :: de_searcher  => null() !< DE search strategy
      class(class_abstract_search_strategy), pointer :: rsm_searcher => null() !< RSM search strategy

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
   subroutine init(this, sys_var, search_strategy_factory)
      implicit none
      class(class_DE_RSM)                                       :: this                    !< A reference to this object
      class(class_system_variables),                 intent(in) :: sys_var                 !< System's variables
      class(class_abstract_search_strategy_factory), intent(in) :: search_strategy_factory !< Search strategy factory

      ! Inner variables
      type(class_ifile)   :: ifile              ! Input file
      character(str_size) :: DESSID             ! DE search strategy ID
      integer             :: np                 ! Population size
      real(8)             :: fh                 ! Hybridization fraction/Initial hybridization fraction
      real(8)             :: fhmin              ! Minimum hybridization factor
      real(8)             :: fhmax              ! Maximum hybridization factor
      character(str_size) :: fhmodel            ! Name of the model for the dynamical calculation of the factor of hybridization
      integer             :: fhm                ! Model for the dynamical calculation of the factor of hybridization
      integer             :: nf                 ! Number of fitting points for response surface adjustment


      ! Getting parameters
      call ifile%init(filename=sys_var%absparfile, field_separator='&')

      call ifile%load()

      call ifile%get_value(        np,   "np")
      call ifile%get_value(    DESSID,   "DE-RSM-de_search_strategy")
      call ifile%get_value(        fh,   "DE-RSM-fh")
      call ifile%get_value(     fhmin,   "DE-RSM-fhmin")
      call ifile%get_value(     fhmax,   "DE-RSM-fhmax")
      call ifile%get_value(   fhmodel,   "DE-RSM-fhm")

      ! Creating DE search strategy
      call search_strategy_factory%create(sys_var, sys_var%absparfile, DESSID, this%de_searcher)


      ! Creating RSM search strategy
      call search_strategy_factory%create(sys_var, sys_var%absparfile, "RSM", this%rsm_searcher)


      ! Getting nf
      associate (rsm_searcher => this%rsm_searcher)

         select type (rsm_searcher)

            type is ( class_RSM_search_strategy )

               nf = rsm_searcher%get_nf()

         end select

      end associate


      ! Allocating resources

      allocate(this%rsm_tag(np)  )
      allocate(this%trial_fit(np))
      allocate(this%curnt_fit(np))

      ! Checking fh model
      if ( trim(fhmodel) == "constant") then
         fhm = 0
      else if ( trim(fhmodel) == "dynamic") then
         fhm = 1
      else
         call sys_var%logger%println("class_DE_RSM: Unknown fh model. Stopping.")

         call mod_mpi_finalize()
      end if

      ! Initializing the hybridization control object
      call this%hybrid_control%init(sys_var, np, nf, fh, fhmin, fhmax, fhm)

   end subroutine



   !> \brief Returns the size of the number of elements of the data vector that
   !! must be shared among threads.
   integer function data_size(this)
      implicit none
      class(class_DE_RSM) :: this !< A reference to this object

      data_size = size(this%rsm_tag)

   end function


   !> \brief Tells MPI how to send each element of data from current thread to 'to_thread'.
   subroutine send(this, i, to_thread)
      implicit none
      class(class_DE_RSM) :: this      !< A reference to this object
      integer, intent(in) :: i         !< Index of the shared data vector
      integer, intent(in) :: to_thread !< Receiver thread

      call mod_mpi_send(to_thread, this%rsm_tag(i)  )
      call mod_mpi_send(to_thread, this%trial_fit(i))
      call mod_mpi_send(to_thread, this%curnt_fit(i))

   end subroutine


   !> \brief Tells MPI how to receive each element of data from 'from_thread' to current thread.
   subroutine recv(this, i, from_thread)
      implicit none
      class(class_DE_RSM) :: this        !< A reference to this object
      integer, intent(in) :: i           !< Index of the shared data vector
      integer, intent(in) :: from_thread !< Sender thread

      call mod_mpi_recv(from_thread, this%rsm_tag(i)  )
      call mod_mpi_recv(from_thread, this%trial_fit(i))
      call mod_mpi_recv(from_thread, this%curnt_fit(i))

   end subroutine


   !> \brief Performs update tasks before the next generation.
   subroutine update(this)
      implicit none
      class(class_DE_RSM) :: this !< A reference to this object

      ! Inner variables
      integer :: i

      do i = 1, size(this%rsm_tag)

         call this%hybrid_control%add(this%rsm_tag(i), this%trial_fit(i), this%curnt_fit(i))

      end do

      call this%hybrid_control%update()

   end subroutine



   !> \brief Generates a trial individual
   subroutine get_trial(this, ind, ehist, x, es)
      implicit none
      class(class_DE_RSM)                   :: this  !< A reference to this object
      integer,                  intent(in)  :: ind   !< Number of the individual of the population
      class(class_ehist),       intent(in)  :: ehist !< Evolution history
      real(8), dimension(:),    intent(out) :: x     !< Trial individual
      integer, optional,        intent(out) :: es    !< Exit status


      integer :: estatus
      integer :: rsmstatus

      if (present(es)) es = 0

      ! Checking if RSM can be applied

      if ( this%hybrid_control%is_rsm_applicable(ehist%g) ) then

         ! rsm_tag stores the return state of application of DE-RSM
         estatus = DE_RSM_RETURN%RSM_APPLIED

         ! Generating a RSM individual

         call this%rsm_searcher%get_trial(ind, ehist, x, rsmstatus)

         ! If RSM fails, generates a pure DE individual
         if ( rsmstatus == 1 ) then

            ! rsm_tag stores the return state of application of DE-RSM
            estatus = DE_RSM_RETURN%DE_APPLIED

            ! Creating the trial individual x
            call this%de_searcher%get_trial(ind, ehist, x)

         end if

      else

         ! rsm_tag stores the return state of application of DE-RSM
         estatus = DE_RSM_RETURN%DE_APPLIED

         call this%de_searcher%get_trial(ind, ehist, x)

      end if

      ! Verifying the constraints. If the individual x is out of range,
      ! another one is created using pure DE
      do while ( is_X_out_of_range(ehist%nu, ehist%xmin, ehist%xmax, x) )

         ! Creating the trial individual x
         call this%de_searcher%get_trial(ind, ehist, x)

         estatus = DE_RSM_RETURN%DE_APPLIED

      end do

      this%rsm_tag(ind) = estatus


   end subroutine

   !> \brief Process the feedback from fitness calculator
   subroutine feed_back(this, ind, ehist, fit, ecode)
      implicit none
      class(class_DE_RSM)                   :: this    !< A reference to this object
      integer,                  intent(in)  :: ind     !< Number of the individual of the population
      class(class_ehist),       intent(in)  :: ehist   !< Evolution history
      real(8),                  intent(in)  :: fit     !< Fitness of the trial individual
      integer,                  intent(in)  :: ecode   !< Error code

      this%curnt_fit(ind) = ehist%fit(ind)
      this%trial_fit(ind) = fit

   end subroutine


end module
