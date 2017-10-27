
!> \brief Provides an abstract class for parallel processing a set of data.

module mod_class_abstract_parallel_processed_data

   use mod_mpi

   implicit none

   ! Makes everything private, except otherwise stated
   private

   !> Provides an abstract class for parallel processing a set of data.
   !! The data may be of any kind, but must be organized as a vector
   !! ranging from 1 to data_size. This class provides a procedure
   !! 'compute_concurrent' that distributes the computation among the available
   !! processors. After that, data must be exchanged using the 'exchange' procedure.
   !! The following methods are deferred to derived implementation classes:
   !! => data_size: defines the maximum length of the data set
   !! => compute: defines how compute each element of the data set
   !! => send: tells mpi how to send each element of data to another thread
   !! => recv: tells mpi how to receive each element of data from another thread
   type, abstract, public :: class_abstract_parallel_processed_data

   contains

      procedure(data_size_interface), deferred, private, pass :: data_size
      procedure(compute_interface),   deferred, private, pass :: compute
      procedure(send_interface),      deferred, private, pass :: send
      procedure(recv_interface),      deferred, private, pass :: recv
      procedure,                                public,  pass :: compute_concurrent
      procedure,                                public,  pass :: exchange

   end type


   ! Defining the interface of the abstract functions
   abstract interface


      !> User defined: Gives the size of the array of data.
      integer function data_size_interface(this)
         import class_abstract_parallel_processed_data
         implicit none
         class(class_abstract_parallel_processed_data) :: this

      end function


      !> User defined. Defines the procedure for calculating each data element.
      subroutine compute_interface(this,i)
         import class_abstract_parallel_processed_data
         implicit none
         class(class_abstract_parallel_processed_data) :: this
         integer, intent(in) :: i

      end subroutine


      !> User defined. Tells MPI how to send each element of data from current thread to 'to_thread'.
      subroutine send_interface(this, i, to_thread)
         import class_abstract_parallel_processed_data
         implicit none
         class(class_abstract_parallel_processed_data) :: this
         integer,                  intent(in) :: i
         integer,                  intent(in) :: to_thread

      end subroutine


      !> User defined. Tells MPI how to receive each element of data from 'from_thread' to current thread.
      subroutine recv_interface(this, i, from_thread)
         import class_abstract_parallel_processed_data
         implicit none
         class(class_abstract_parallel_processed_data) :: this
         integer,                  intent(in) :: i
         integer,                  intent(in) :: from_thread

      end subroutine

   end interface


contains


   !> Executes in parallel the procedure defined in compute(i) for each element of data.
   subroutine compute_concurrent(this)
      implicit none
      class(class_abstract_parallel_processed_data) :: this

      integer :: i

      do i = 1, this%data_size()

         if ( mpi%iproc == thread_map(i) ) call this%compute(i)

      end do

   end subroutine


   !> Exchange data among threads.
   subroutine exchange(this)
      implicit none
      class(class_abstract_parallel_processed_data) :: this

      integer :: i
      integer :: from_thread
      integer :: to_thread

      call mod_mpi_barrier()

      do i = 1, this%data_size()

         ! Data of index 'i' was calculated by thread=thread_map(i)
         from_thread = thread_map(i)

         ! If the current thread is the same thread that calculated data i, send data to others thread
         if ( mpi%iproc == from_thread ) then

            do to_thread = 0, mpi%nproc-1

               if ( to_thread /= from_thread ) call this%send(i, to_thread)

            end do

         ! Otherwise, receive data
         else

            call this%recv(i, from_thread)

         end if

      end do

      call mod_mpi_barrier()

   end subroutine


   ! Defines the rule for mapping each element of data to available threads.
   integer function thread_map(i)
      implicit none
      integer, intent(in) :: i

      ! This rule leaves all the work to slaves.
      thread_map = mod(i, mpi%nproc-1) + 1

      ! This rule shares the work among all threads, including master thread.
      !thread_map = mod(i, mpi%nproc)

   end function


end module
