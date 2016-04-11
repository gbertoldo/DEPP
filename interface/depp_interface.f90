!> \brief Creates an interface to DEPP optimizer
module depp_interface
   implicit none

   character (len=200),  private :: arqfit = "" !< name of file for the fitness

contains

   !> \brief Reads the parameters from DEPP
   subroutine depp_get_parameters(np, xopt, xname, x, ind, sname)
      implicit none
      integer,               intent(in) ::         np !< List of variables
      integer,               intent(in) ::   xopt(np) !< Checker of variables to be read from DEPP
      character(len=10),     intent(in) ::  xname(np) !< Name of variables
      real(8),            intent(inout) ::      x(np) !< Values of the variables
      integer,              intent(out) ::        ind !< number of the individual
      character(len=200),   intent(out) ::      sname !< name of the simulation


      ! Parameters
      integer, parameter :: unt = 5 !< Unit from which the data are read (5 = std input)


      ! Inner variables
      integer ::        i ! Dummy index
      integer ::       nu ! Number of variables to be read from DEPP
      integer ::  depp_nu ! Number of variables received from DEPP

      character (len=10) :: depp_xname ! Name of the variables received from DEPP


      ! Initializing global variables

      arqfit = "" ! name of file for the fitness


      ! Number of variables to be read from DEPP

      nu = sum(xopt)

      if ( nu == 0 ) then

         return

      end if

      ! Reading the filename where the fitness will be saved

      read(unt,*) arqfit

      if ( trim(adjustl(arqfit)) == "" ) then

         write(*,*) "depp_get_parameters: empty output file name. Stopping"

      end if

      ! Reading the simulation name
      read(unt,*) sname


      ! Reading the individual number
      read(unt,*) ind


      ! Reading the number of unknowns
      read(unt,*) depp_nu


      ! Checks if the number of unknowns of the user is the same of the DEPP
      if ( depp_nu /= nu ) then

         write(*,*) "depp_get_parameters: wrong number of parameters. Stopping..."

         stop

      end if


      
      read(unt,*)
      read(unt,*) !=========== VARIABLES OF OPTIMIZATION =====================
      read(unt,*) !            X                       Variable name

      ! Reading the unknowns and their names
      do i = 1, np

         if ( xopt(i) == 1 ) then

            read(unt,*) x(i), depp_xname

            if ( trim(adjustl(depp_xname)) /= trim(adjustl(xname(i))) ) then

               write(*,*) "depp_get_parameters: wrong variable name. Stopping..."

               stop

            end if

         end if

      end do


   end subroutine depp_get_parameters



   !> \brief Saves the fitness function to a file
   subroutine depp_save_fitness(fitness, estatus, fitness_name)
      implicit none
      real(8), intent(in) :: fitness !< Fitness for a given individual
      integer, intent(in) :: estatus !< Exit status ( 0 = success, 1 = failure, 2 = generate another individual)
      character (len=*), intent(in) :: fitness_name !< Name of the fitness function


      if ( trim(adjustl(arqfit)) /= "" ) then

         open(10, file = trim(adjustl(arqfit)) )

         write(10,"(ES23.16,A)") fitness, " = " // trim(adjustl(fitness_name))

         write(10,"(I23,A)") estatus, " = Exit status ( 0 = success, 1 = failure, 2 = generate another individual)"

         close(10)

      end if

   end subroutine depp_save_fitness


end module depp_interface
