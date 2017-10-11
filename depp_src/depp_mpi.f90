!> \brief This module stores the global variables of MPI
module mod_mpi
   implicit none

   include 'mpif.h'

   integer :: status(mpi_status_size)  !< mpi: vector with information and source tag
   integer :: iproc                    !< mpi: identification number of the process
   integer :: code                     !< mpi: status code
   integer :: nproc                    !< mpi: number of processes
   integer :: tag                      !< mpi: message label
   integer :: comm                     !< mpi: MPI communicator


contains

   !> \brief Initializes MPI module
   subroutine initialize_mpi_module()
      implicit none

      ! Initializing MPI (from now on the code is parallel)
      call mpi_init(code)


      ! Initializing MPI variables
      comm = mpi_comm_world
      tag = 42


      ! Checking for MPI initialization errors
      if (code /= mpi_success) then
         write(*,*) " =====  Error in the MPI initialization. Stopping...  ====="
         call mpi_abort(comm, code, code)
      endif


      ! Getting the total number of processors
      call mpi_comm_size(comm, nproc, code)


      ! Getting the ID number of each processor
      call mpi_comm_rank(comm, iproc, code)


   end subroutine


end module
