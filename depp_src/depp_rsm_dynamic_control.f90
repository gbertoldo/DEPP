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

   ! Model for the dynamical calculation of the factor of hybridization
   integer, private :: fhm0

   ! Register if RSM was applied with success (1) or not (0)
   real(8), allocatable, dimension(:), private :: r_rsm


contains

   ! \brief Initializes RSM Dynamic Control module
   subroutine initialize_rsm_dynamic_control(np, fh, fhm)
      implicit none
      integer, intent(in) :: np  !< Population size
      real(8), intent(in) :: fh  !< Initial hybridization factor
      integer, intent(in) :: fhm !< Model for the dynamical calculation of the factor of hybridization

      nps = np

      fh0 = fh

      fhm0 = fhm

      allocate(r_rsm(np))

      r_rsm = -1.d0

      ireg = 0

   end subroutine



   ! \brief Add a result and update RSM Dynamic Control
   subroutine add_to_rsm_dynamic_control(rsm_tag, xfit, fit)
      implicit none
      integer, intent(in) :: rsm_tag !< 0=DE, 1=RSM
      real(8), intent(in) :: xfit    !< fitness of the trial individual
      real(8), intent(in) :: fit     !< fitness of a given individual of the population

      ! If RSM was applied
      if ( rsm_tag == 1 ) then

         ! Getting the index of the current register
         ireg = idx(ireg+1,nps)


         ! If RSM was applied with success
         if ( xfit > fit ) then

            r_rsm(ireg) = 1.d0

         else

            r_rsm(ireg) = 0.d0

         end if

      end if

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

      select case (fhm0)

         ! Constant hybridization factor
         case (0)

              get_hybridization_factor = fh0

         ! Variable hybridization factor
         case (1)

            if ( rsm_p_success() > 0.d0 ) then

               get_hybridization_factor =  rsm_p_success()

            else

               get_hybridization_factor = fh0

            end if

         case default

              get_hybridization_factor = fh0

      end select

   end function


end module
