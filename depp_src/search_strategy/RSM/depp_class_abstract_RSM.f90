!> \brief Provides an abstract class for fitting a response surface and returning its optimizer

module mod_class_abstract_RSM

   implicit none

   !> \brief Interface for a RSM object
   type, abstract, public :: class_abstract_RSM

   contains

      procedure(dim_interface),           deferred, public, pass :: dim             !< Minimum number of points necessary to fit the response surface
      procedure(fit_interface),           deferred, public, pass :: fit             !< Fits the response surface to the data
      procedure(P_interface),             deferred, public, pass :: P               !< Returns the value of the fitted response surface
      procedure(get_optimizer_interface), deferred, public, pass :: get_optimizer   !< Returns the response surface optimizer

   end type

   abstract interface

      integer function dim_interface(this, n)
         import class_abstract_RSM
         implicit none
         class(class_abstract_RSM)     :: this
         integer, optional, intent(in) :: n

      end function

      subroutine fit_interface(this, dm, fm, wm, es)
         import class_abstract_RSM
         implicit none
         class(class_abstract_RSM)            :: this
         real(8), dimension(:,:), intent(in)  :: dm   !< Design matrix (each row is an x point)
         real(8), dimension(:),   intent(in)  :: fm   !< 'Measures of f'
         real(8), dimension(:),   intent(in)  :: wm   !< Weight of 'Measures of f'
         integer,                 intent(out) :: es   !< Exit status: 0 = success, 1 = failure

      end subroutine

      real(8) function P_interface(this, x)
         import class_abstract_RSM
         implicit none
         class(class_abstract_RSM)         :: this
         real(8), dimension(:), intent(in) :: x    !< Independent variables

      end function

      subroutine get_optimizer_interface(this, dm, x, ko, es)
         import class_abstract_RSM
         implicit none
         class(class_abstract_RSM)            :: this
         real(8), dimension(:,:), intent(in)  ::   dm !< Design matrix (each row is an x point)
         real(8), dimension(:),   intent(out) ::    x !< Coordinates of the optimizer
         integer,                 intent(out) ::   ko !< ko: -1 = minimizer, 0 = saddle point, 1 = maximizer
         integer,                 intent(out) ::   es !< Exit status: 0 = success, 1 = failure

      end subroutine

   end interface

end module
