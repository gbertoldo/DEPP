!> \brief This module stores the global variables of MPI
module mod_mpi

   use mpi

   implicit none


   ! Makes everything private, except otherwise stated
   private


   ! Public procedures
   public :: mod_mpi_init,     &
             mod_mpi_barrier,  &
             mod_mpi_abort,    &
             mod_mpi_finalize, &
             mod_mpi_send,     &
             mod_mpi_recv


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


   ! Facade for sending messages
   interface mod_mpi_send

      module procedure mod_mpi_send_integer
      module procedure mod_mpi_send_integer_vector
      module procedure mod_mpi_send_double
      module procedure mod_mpi_send_double_vector

   end interface


   ! Facade for receiving messages
   interface mod_mpi_recv

      module procedure mod_mpi_recv_integer
      module procedure mod_mpi_recv_integer_vector
      module procedure mod_mpi_recv_double
      module procedure mod_mpi_recv_double_vector

   end interface

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


   !> Facade for sending an integer
   subroutine mod_mpi_send_integer(to_thread, dta)
      implicit none
      integer, intent(in) :: to_thread !< Number of the receiver thread
      integer, intent(in) :: dta       !< Data

      call mpi_send(dta, 1, mpi_integer, to_thread, mpio%tag, mpio%comm, mpio%code)

   end subroutine


   !> Facade for sending a vector of integers
   subroutine mod_mpi_send_integer_vector(to_thread, dta)
      implicit none
      integer, intent(in) :: to_thread !< Number of the receiver thread
      integer, intent(in) :: dta(:)    !< Data

      call mpi_send(dta, size(dta), mpi_integer, to_thread, mpio%tag, mpio%comm, mpio%code)

   end subroutine


   !> Facade for sending a double
   subroutine mod_mpi_send_double(to_thread, dta)
      implicit none
      integer, intent(in) :: to_thread !< Number of the receiver thread
      real(8), intent(in) :: dta       !< Data

      call mpi_send(dta, 1, mpi_double_precision, to_thread, mpio%tag, mpio%comm, mpio%code)

   end subroutine


   !> Facade for sending a vector of doubles
   subroutine mod_mpi_send_double_vector(to_thread, dta)
      implicit none
      integer, intent(in) :: to_thread !< Number of the receiver thread
      real(8), intent(in) :: dta(:)    !< Data

      call mpi_send(dta, size(dta), mpi_double_precision, to_thread, mpio%tag, mpio%comm, mpio%code)

   end subroutine


   !> Facade for receiving an integer
   subroutine mod_mpi_recv_integer(from_thread, dta)
      implicit none
      integer, intent(in) :: from_thread !< Number of the sender thread
      integer, intent(in) :: dta         !< Data

      call mpi_recv(dta, 1, mpi_integer, from_thread, mpio%tag, mpio%comm, mpio%status, mpio%code)

   end subroutine


   !> Facade for receiving a vector of integers
   subroutine mod_mpi_recv_integer_vector(from_thread, dta)
      implicit none
      integer, intent(in) :: from_thread !< Number of the sender thread
      integer, intent(in) :: dta(:)      !< Data

      call mpi_recv(dta, size(dta), mpi_integer, from_thread, mpio%tag, mpio%comm, mpio%status, mpio%code)

   end subroutine


   !> Facade for receiving a double
   subroutine mod_mpi_recv_double(from_thread, dta)
      implicit none
      integer, intent(in) :: from_thread !< Number of the sender thread
      real(8), intent(in) :: dta         !< Data

      call mpi_recv(dta, 1, mpi_double_precision, from_thread, mpio%tag, mpio%comm, mpio%status, mpio%code)

   end subroutine


   !> Facade for receiving a vector of doubles
   subroutine mod_mpi_recv_double_vector(from_thread, dta)
      implicit none
      integer, intent(in) :: from_thread !< Number of the sender thread
      real(8), intent(in) :: dta(:)      !< Data

      call mpi_recv(dta, size(dta), mpi_double_precision, from_thread, mpio%tag, mpio%comm, mpio%status, mpio%code)

   end subroutine


end module
