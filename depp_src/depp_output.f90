!> \brief Contains subroutines for writing output data.
module output

   use mod_class_timer

   use mod_class_system_variables

   implicit none

contains

   !============================================================================

   !> \brief Saves data about the failure in the calculation of the fitness
   subroutine save_fitness_failure(nu, g, ind, sys_var, sname, x, estatus)
      implicit none
      integer, intent(in) ::  nu !< number of unknowns
      integer, intent(in) ::   g !< number of the generation
      integer, intent(in) :: ind !< number of the individual
      integer, intent(in) :: estatus   !< exit status (0=success; 1=failure; 2=generate another individual)

      class(class_system_variables), intent(in) :: sys_var
      character(len=*), intent(in) :: sname     !< simulations name

      real(8), intent(in) :: x(nu) !< unknowns


      ! Inner variables

      integer :: i ! Dummy index
      character(3) :: str1 ! Auxiliary string 1
      character(3) :: str2 ! Auxiliary string 2
      character(250) :: fout ! Output file name


      ! Creating the name of the output file

      call convert_int_to_char3(g,str1)

      call convert_int_to_char3(ind,str2)

      fout = trim(adjustl(sys_var%absfolderout)) // "fitness_failures_i" // str2 // ".txt"

      open(10, file = fout, position = "append" )

      if (len(trim(adjustl(sname))) > 23) then
         write(10,*) trim(adjustl(sname)), " = sname:    simulation name"
      else
         write(10,"(A23,A)") trim(adjustl(sname)), " = sname:    simulation name"
      end if

      write(10,"(I23,A)") g, " = g:        generation number"
      write(10,"(I23,A)") ind, " = ind:      individual number"
      write(10,"(I23,A)") estatus, " = estatus:  exit status"

      do i = 1, nu

         write(10,"(ES23.16,A,I2,A)") x(i), " = x(", i, "):"

      end do

      write(10,*)

      close(10)

   contains

      subroutine convert_int_to_char3(nmbr, char3) ! last parameter is output
         implicit none
         character(3), intent(out) :: char3
         integer, intent(in) :: nmbr

         if (nmbr < 10) then
            write(char3,"('00',i1)") nmbr
         else if (nmbr < 100) then
            write(char3,"('0',i2)") nmbr
         else
            write(char3,"(i3)") nmbr
         end if

      end subroutine convert_int_to_char3

   end subroutine save_fitness_failure


end module output
