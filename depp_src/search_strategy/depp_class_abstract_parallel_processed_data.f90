
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

      procedure(data_size_interface), deferred, private, pass :: data_size          !< Gives the size of the array of data
      procedure(compute_interface),   deferred, private, pass :: compute            !< Defines the procedure for calculating each data element
      procedure(send_interface),      deferred, private, pass :: send               !< Tells MPI how to send each element of data from current thread to 'to_thread'
      procedure(recv_interface),      deferred, private, pass :: recv               !< Tells MPI how to receive each element of data from 'from_thread' to current thread
      procedure(update_interface),    deferred, private, pass :: update             !< Perform calculations after parallel computation and data synchronization
      !! and data synchronization
      procedure,                                public,  pass :: compute_concurrent !< Executes in parallel the procedure defined in compute(i) for each element of data
      procedure,                                private, pass :: exchange           !< Exchange data among threads

   end type


   ! Defining the interface of the abstract functions
   abstract interface


      !> \brief User defined: Gives the size of the array of data.
      integer function data_size_interface(this)
         import class_abstract_parallel_processed_data
         implicit none
         class(class_abstract_parallel_processed_data) :: this !< A reference to this object

      end function


      !> \brief User defined. Defines the procedure for calculating each data element.
      subroutine compute_interface(this,i)
         import class_abstract_parallel_processed_data
         implicit none
         class(class_abstract_parallel_processed_data) :: this !< A reference to this object
         integer, intent(in) :: i

      end subroutine


      !> \brief User defined. Tells MPI how to send each element of data from current thread to 'to_thread'.
      subroutine send_interface(this, i, to_thread)
         import class_abstract_parallel_processed_data
         implicit none
         class(class_abstract_parallel_processed_data) :: this      !< A reference to this object
         integer,                           intent(in) :: i         !< Index of the shared data
         integer,                           intent(in) :: to_thread !< Receiver thread

      end subroutine


      !> \brief User defined. Tells MPI how to receive each element of data from 'from_thread' to current thread.
      subroutine recv_interface(this, i, from_thread)
         import class_abstract_parallel_processed_data
         implicit none
         class(class_abstract_parallel_processed_data) :: this        !< A reference to this object
         integer,                           intent(in) :: i           !< Index of the shared data
         integer,                           intent(in) :: from_thread !< Sender thread

      end subroutine


      !> \brief User defined. This function may be used to perform extra calculations after
      !! parallel computation and data synchronization.
      subroutine update_interface(this)
         import class_abstract_parallel_processed_data
         implicit none
         class(class_abstract_parallel_processed_data) :: this !< A reference to this object

      end subroutine

   end interface


contains


   !> Executes in parallel the procedure defined in compute(i) for each element of data.
   subroutine compute_concurrent(this)
      implicit none
      class(class_abstract_parallel_processed_data) :: this !< A reference to this object

      integer :: i

      do i = 1, this%data_size()

         if ( mpio%iproc == thread_map(i) ) call this%compute(i)

      end do

      ! Exchanging data among threads
      call this%exchange()

   end subroutine


   !> Exchange data among threads.
   subroutine exchange(this)
      implicit none
      class(class_abstract_parallel_processed_data) :: this !< A reference to this object

      integer :: i
      integer :: from_thread
      integer :: to_thread

      call mod_mpi_barrier()

      do i = 1, this%data_size()

         ! Data of index 'i' was calculated by thread=thread_map(i)
         from_thread = thread_map(i)

         ! If the current thread is the same thread that calculated data i, send data to others thread
         if ( mpio%iproc == from_thread ) then

            do to_thread = 0, mpio%nproc-1

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
      !thread_map = mod(i, mpio%nproc-1) + 1

      ! This rule shares the work among all threads, including master thread.
      thread_map = mod(i, mpio%nproc)

   end function


end module
