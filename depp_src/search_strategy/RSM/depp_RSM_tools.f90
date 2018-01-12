!> \brief Provides common procedures to all response surfaces
module mod_RSM_tools
   implicit none

contains

   !> \brief Determines if the optimum is a maximizer, minimizer or a saddle point
   subroutine get_kind_of_optimum(m, n, dm, x, f, ko)
      implicit none
      integer,                 intent(in)  :: m    !< Number of 'measures'
      integer,                 intent(in)  :: n    !< Number of variables
      real(8), dimension(m,n), intent(in)  :: dm   !< Design matrix (each row is an x point)
      real(8), dimension(n),   intent(in)  :: x    !< Critical point
      integer,                 intent(out) :: ko   !< ko: -1 = minimizer, 0 = saddle point, 1 = maximizer

      interface

         real(8) function f(x)
            implicit none
            real(8), dimension(:) :: x

         end function

      end interface


      ! Inner variables
      integer :: k
      real(8) :: fc
      real(8) :: Pc
      real(8) :: Pmin
      real(8) :: Pmax
      real(8) :: xmin(n)
      real(8) :: xmax(n)

      xmin = x
      xmax = x

      Pmax = -huge(1.d0)
      Pmin =  huge(1.d0)

      fc = f(x)


      ! Searching for maximum and minimum

      do k = 1, m

         Pc = f(dm(k,:))

         if ( Pc < Pmin ) then

            Pmin = Pc

            xmin = dm(k,:)

         end if

         if ( Pmax < Pc ) then

            Pmax = Pc

            xmax = dm(k,:)

         end if

      end do


      ! Selecting ko

      if ( fc <= Pmin) then

         ko = -1

      else if ( Pmax <= fc ) then

         ko = 1

      else

         ko = 0

      end if

   end subroutine

end module
