!> \brief Creates an interface to DEPP optimizer
module depp_interface
   implicit none

   character (len=200),  private :: arqfit = "" !< name of file for the fitness

contains

   !> \brief Reads the parameters from DEPP
   subroutine depp_get_parameters(x)
      implicit none
      real(8), allocatable, intent(out) :: x(:) !< Values of the variables
      
      ! Inner variables
      integer             :: i     ! Dummy index
      integer             :: nu    ! Number of unknowns
      character(len=2000) :: ifile ! Input file

      ! Getting the input file name
      call get_command_argument(1, ifile)
      
      ! Openning input file
      open(10,file=trim(ifile))

      ! Initializing global variables

      arqfit = "" ! name of file for the fitness

      ! Reading the filename where the fitness will be saved

      read(10,*) arqfit

      if ( trim(adjustl(arqfit)) == "" ) then

         write(*,*) "depp_get_parameters: empty output file name. Stopping"

      end if

      ! Reading the number of unknowns
      read(10,*) nu

      ! Allocating memory
      allocate(x(nu))

      ! Reading the unknowns and their names
      do i = 1, nu

         read(10,*) x(i)

      end do
      
      close(10)


   end subroutine depp_get_parameters



   !> \brief Saves the fitness function to a file
   subroutine depp_save_fitness(fitness, estatus)
      implicit none
      real(8), intent(in) :: fitness !< Fitness for a given individual
      integer, intent(in) :: estatus !< Exit status ( 0 = success, 1 = failure, 2 = generate another individual)


      if ( trim(adjustl(arqfit)) /= "" ) then

         open(10, file = trim(adjustl(arqfit)) )

         write(10,"(ES23.16,A)") fitness, " = Fitness" 

         write(10,"(I23,A)") estatus, " = Exit status ( 0 = success, 1 = failure, 2 = generate another individual)"

         close(10)

      end if

   end subroutine depp_save_fitness


end module depp_interface
