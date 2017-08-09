!> \brief Contains tools for controlling dynamically the fraction
!! of the population generated using RSM
module rsm_dynamic_control

   implicit none

   ! Population size
   integer, private :: nps

   ! Index of the current register
   integer, private :: ireg

   ! Initial factor of hybridization
   real(8), private :: fh0

   ! Minimum factor of hybridization
   real(8), private :: fhmin0

   ! Maximum factor of hybridization
   real(8), private :: fhmax0

   ! Model for the dynamical calculation of the factor of hybridization
   integer, private :: fhm0

   ! Register if RSM was applied with success (1) or not (0)
   real(8), allocatable, dimension(:), private :: r_rsm

   ! Defining a return code for application of DE-RSM
   type, private :: DE_RSM_RETURN_CODE

      integer ::                   DE_APPLIED
      integer ::                  RSM_APPLIED
      integer :: DE_APPLIED_AFTER_RSM_FAILURE
      integer :: BLACK_BOX_EVALUATION_FAILURE

   end type

   type(DE_RSM_RETURN_CODE), parameter, public :: DE_RSM_RETURN = DE_RSM_RETURN_CODE(0,1,2,3)

contains

   ! \brief Initializes RSM Dynamic Control module
   subroutine initialize_rsm_dynamic_control(np, fh, fhmin, fhmax, fhm)
      implicit none
      integer, intent(in) :: np    !< Population size
      real(8), intent(in) :: fh    !< Initial hybridization factor
      real(8), intent(in) :: fhmin !< Minimum hybridization factor
      real(8), intent(in) :: fhmax !< Maximum hybridization factor
      integer, intent(in) :: fhm   !< Model for the dynamical calculation of the factor of hybridization

      nps = np

      fh0 = fh

      fhmin0 = fhmin

      fhmax0 = fhmax

      fhm0 = fhm

      allocate(r_rsm(np))

      r_rsm = -1.d0

      ireg = 0

   end subroutine



   ! \brief Add a result and update RSM Dynamic Control
   subroutine add_to_rsm_dynamic_control(rsm_tag, xfit, fit)
      implicit none
      integer, intent(in) :: rsm_tag !< Stores the return state of application of DE-RSM
      real(8), intent(in) :: xfit    !< fitness of the trial individual
      real(8), intent(in) :: fit     !< fitness of a given individual of the population

      ! Checking DE-RSM status
      select case (rsm_tag)


         ! If DE was applied, do nothing
         case (DE_RSM_RETURN%DE_APPLIED)



         ! If RSM was applied, evaluate the success of the application
         case (DE_RSM_RETURN%RSM_APPLIED)

            ! Getting the index of the current register
            ireg = idx(ireg+1,nps)

            ! If RSM was applied with success
            if ( xfit > fit ) then

               r_rsm(ireg) = 1.d0

            else

               r_rsm(ireg) = 0.d0

            end if


         ! If RSM was not applied, its is not possible to  evaluate the success of improving the fitness function
         case (DE_RSM_RETURN%DE_APPLIED_AFTER_RSM_FAILURE)


         ! If black box evaluation failed, do nothing
         case (DE_RSM_RETURN%BLACK_BOX_EVALUATION_FAILURE)


         case default

            write(*,*) "add_to_rsm_dynamic_control: unknown return code. Stopping."
            stop

      end select


   end subroutine




   !> \brief Returns the probability of success of RSM
   real(8) function rsm_p_success()
      implicit none

      ! Inner variables
      integer :: i

      rsm_p_success = -1.d0

      ! Checking if a sufficient number of trials exist for calculating the probability
      do i = 1, nps

         if ( r_rsm(i) < 0.d0 ) return

      end do

      rsm_p_success = sum(r_rsm) / dble(nps)

   end function




   !> \brief Returns the index of the current register
   integer function idx(i, nps)
      implicit none
      integer, intent(in) :: i
      integer, intent(in) :: nps

      idx = mod(i,nps)

      if (idx==0) idx = nps

   end function



   !> \brief Calculates the hybridization factor according to a prescribed model.
   real(8) function get_hybridization_factor()
      implicit none

      ! Inner variables
      real(8) :: ps

      select case (fhm0)

         ! Constant hybridization factor
         case (0)

              get_hybridization_factor = fh0

         ! Variable hybridization factor
         case (1)

            ps = rsm_p_success()

            if ( 0.d0 <= ps .and. ps <= fhmin0 ) then

               get_hybridization_factor =  fhmin0

            else if ( fhmin0 < ps .and. ps < fhmax0 ) then

               get_hybridization_factor =  ps

            else if ( fhmax0 <= ps .and. ps <= 1.d0 ) then

               get_hybridization_factor =  fhmax0

            else

               get_hybridization_factor = fh0

            end if

         case default

              get_hybridization_factor = fh0

      end select

   end function


end module
