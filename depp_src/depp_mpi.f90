!> \brief This module stores the global variables of MPI
module mod_mpi

   use mpi

   implicit none

   !include 'mpif.h'


   ! Makes everything private, except otherwise stated
   !private


   ! MPI main variables
   type, public :: class_mpi

      integer :: status(mpi_status_size)  !< mpi: vector with information and source tag
      integer :: iproc                    !< mpi: identification number of the process
      integer :: code                     !< mpi: status code
      integer :: nproc                    !< mpi: number of processes
      integer :: tag                      !< mpi: message label
      integer :: comm                     !< mpi: MPI communicator
      logical :: master                   !< true if master

   end type


   ! MPI object
   type(class_mpi), public :: mpio


contains


   !> \brief Initializes MPI module
   subroutine mod_mpi_init()
      implicit none


      ! Initializing MPI
      call mpi_init(mpio%code)


      ! Initializing MPI variables
      mpio%comm = mpi_comm_world
      mpio%tag  = 42


      ! Checking for MPI initialization errors
      if (mpio%code /= mpi_success) then

         write(*,*) " =====  Error in the MPI initialization. Stopping...  ====="

         call mpi_abort(mpio%comm, mpio%code, mpio%code)

      endif


      ! Getting the total number of processors
      call mpi_comm_size(mpio%comm, mpio%nproc, mpio%code)


      ! Getting the ID number of each processor
      call mpi_comm_rank(mpio%comm, mpio%iproc, mpio%code)


      ! Defining if master or not
      if (mpio%iproc==0) then

         mpio%master = .true.

      else

         mpio%master = .false.

      end if


   end subroutine


   !> \brief A barrier to all processors
   subroutine mod_mpi_barrier()
      implicit none

      call mpi_barrier(mpio%comm, mpio%code)

   end subroutine


   !> \brief MPI abort
   subroutine mod_mpi_abort()
      implicit none

      call mpi_abort(mpio%comm, mpio%code, mpio%code)

   end subroutine


   !> \brief MPI finalize
   subroutine mod_mpi_finalize()
      implicit none

      call mpi_finalize(mpio%code)

   end subroutine


end module
