!> \brief This module stores the global variables of MPI
module mod_mpi
   implicit none

   include 'mpif.h'

   ! MPI main variables
   type :: mpi_var

      integer :: status(mpi_status_size)  !< mpi: vector with information and source tag
      integer :: iproc                    !< mpi: identification number of the process
      integer :: code                     !< mpi: status code
      integer :: nproc                    !< mpi: number of processes
      integer :: tag                      !< mpi: message label
      integer :: comm                     !< mpi: MPI communicator

   end type

   type(mpi_var) :: mpi

contains

   !> \brief Initializes MPI module
   subroutine mod_mpi_init()
      implicit none

      ! Initializing MPI (from now on the code is parallel)
      call mpi_init(mpi%code)


      ! Initializing MPI variables
      mpi%comm = mpi_comm_world
      mpi%tag = 42


      ! Checking for MPI initialization errors
      if (mpi%code /= mpi_success) then
         write(*,*) " =====  Error in the MPI initialization. Stopping...  ====="
         call mpi_abort(mpi%comm, mpi%code, mpi%code)
      endif


      ! Getting the total number of processors
      call mpi_comm_size(mpi%comm, mpi%nproc, mpi%code)


      ! Getting the ID number of each processor
      call mpi_comm_rank(mpi%comm, mpi%iproc, mpi%code)


   end subroutine


   !> \brief A barrier to all processors
   subroutine mod_mpi_barrier()
      implicit none

      call mpi_barrier(mpi%comm, mpi%code)

   end subroutine


   !> \brief MPI abort
   subroutine mod_mpi_abort()
      implicit none

      call mpi_abort(mpi%comm, mpi%code, mpi%code)

   end subroutine


   !> \brief MPI finalize
   subroutine mod_mpi_finalize()
      implicit none

      call mpi_finalize(mpi%code)

   end subroutine


end module
