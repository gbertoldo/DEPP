
!> \brief Defines a stop condition class which stops based on the distance of the individuals.

module mod_class_p_measure_stop_condition

   use mod_class_abstract_stop_condition
   use mod_class_ehist
   use mod_class_system_variables
   use mod_class_ifile
   use mod_string

   implicit none


   ! Makes everything private, except otherwise stated
   private

   !> \brief Class for creating stop condition objects based on the distance of the individuals
   type, public, extends(class_abstract_stop_condition) :: class_p_measure_stop_condition

      private
      integer :: kpm       !< Kind of population convergence measure
      real(8) :: ptol      !< Tolerance for the population convergence measure
      logical :: stopflag  !< Stop condition flag
      real(8) :: pcm       !< Population convergence measure
      integer :: verbosity !< Verbosity level for log

   contains

      procedure, public, pass :: init                        !< Constructor
      procedure, public, pass :: compute_stop_condition      !< Computes stop condition
      procedure, public, pass :: is_stop_condition_satisfied !< Checks if stop condition is satisfied
      procedure, public, pass :: convergence_info            !< Returns a string containing convergence information
      procedure, public, pass :: final_convergence_info      !< Returns a string containing final convergence information

   end type

contains


   !> \brief Constructor
   subroutine init(this, sys_var)
      implicit none
      class(class_p_measure_stop_condition)      :: this    !< A reference to this object
      class(class_system_variables),  intent(in) :: sys_var !< System's variables

      ! Inner variables
      type(class_ifile) :: ifile


      ! Reading configuration file
      call ifile%init(filename=trim(sys_var%absparfile), field_separator="&")
      call ifile%load()
      call ifile%get_value(this%kpm,  "kpm")
      call ifile%get_value(this%ptol,"ptol")
      call ifile%get_value(this%verbosity,"verbosity")

      this%stopflag = .false.

      this%pcm = huge(1.d0)

   end subroutine


   !> \brief Computes the stop condition
   subroutine compute_stop_condition(this, ehist)
      implicit none
      class(class_p_measure_stop_condition) :: this  !< A reference to this object
      class(class_ehist),        intent(in) :: ehist !< Evolution history

      if ( 0 < ehist%g ) then

         call get_p_measure(this%kpm, ehist%nu, ehist%np, ehist%xmin, ehist%xmax, ehist%pop, this%pcm)

         if ( this%pcm <= this%ptol ) this%stopflag = .true.

      end if

   end subroutine


   !> \brief Checks if the stop condition was reached.
   logical function is_stop_condition_satisfied(this)
      implicit none
      class(class_p_measure_stop_condition) :: this !< A reference to this object

      is_stop_condition_satisfied = this%stopflag

   end function


   !> \brief Returns the convergence information
   function convergence_info(this) result(str)
      implicit none
      class(class_p_measure_stop_condition) :: this !< A reference to this object
      character(len=:), allocatable         :: str  !< String containing convergence information

      ! Inner variables
      character(len=str_size) caux

      caux = ""

      select case (this%verbosity)
          case (0)

          case (1)

             write(caux,"(A, ES14.7, A, ES14.7, A)") &
                "   --->  Population convergence measure: ", &
                this%pcm, ". Tolerance: ", this%ptol, "."

          case (2)

             write(caux,"(A, ES14.7, A, ES14.7, A)") &
                "   --->  Population convergence measure: ", &
                this%pcm, ". Tolerance: ", this%ptol, "."

          case default

      end select

      str = trim(caux)

   end function


   !> \brief Returns the final convergence information
   function final_convergence_info(this) result(str)
      implicit none
      class(class_p_measure_stop_condition) :: this !< A reference to this object
      character(len=:), allocatable         :: str  !< String containing convergence information

      ! Inner variables
      character(len=str_size) caux

      caux = ""

      write(caux,"(A, ES23.16, A)") trim(caux),  this%pcm, " : population convergence measure"//char(10)
      write(caux,"(A, ES23.16, A)") trim(caux), this%ptol, " : tolerance for population convergence measure"

      str = trim(caux)

   end function


   !> \brief Calculates the P-measure, i. e. the population
   !! convergence measure of FEOKTISTOV "Differential evolution" 2006
   subroutine get_p_measure(kpm, nu, np, xmin, xmax, pop, pm)
      implicit none
      integer, intent(in)  :: kpm        !< Kind of P-measure (1=dimensional, 2=dimensionless)
      integer, intent(in)  :: nu         !< Dimension of the problem
      integer, intent(in)  :: np         !< Size of the population
      real(8), intent(in)  :: xmin(nu)   !< lower boundary constraints
      real(8), intent(in)  :: xmax(nu)   !< upper boundary constraints
      real(8), intent(in)  :: pop(np,nu) !< Population
      real(8), intent(out) :: pm         !< P-measure

      ! Inner variables

      integer :: j ! Dummy index

      real(8) :: raux ! Auxiliary variable

      real(8), dimension(nu)    ::   pb ! Mean individual
      real(8), dimension(np,nu) :: apop ! Auxiliary population


      if ( kpm == 1 ) then

         ! Dimensional population

         apop = pop

      else

         ! Calculation of the dimensionless population

         do j = 1, np

            apop(j,:) = ( pop(j,:) - xmin ) / ( xmax - xmin )

         end do

      end if

      ! Calculating the mean individual

      pb = 0.d0

      do j = 1, np

         pb = pb + apop(j,:)

      end do

      pb = pb / dble(np)


      ! Calculating the P-measure

      pm = 0.d0

      do j = 1, np

         raux = norml2( apop(j,:) - pb, nu )

         if ( raux > pm ) pm = raux

      end do

   end subroutine get_p_measure




   !> \brief Calculates the norm 2 of a vector
   function norml2(vec, n)
      implicit none
      integer :: n      !< dimension of the vector
      real(8) :: norml2 !< norm of the vector
      real(8) :: vec(n) !< vector

      ! Inner variables

      integer :: i
      real(8) :: sum

      sum = 0.d0

      do i = 1, n

         sum = sum + vec(i)**2

      end do

      norml2 = dsqrt(sum)

      return

   end function norml2

end module
