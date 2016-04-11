!> \brief Keeps the history of the generations
module history
   implicit none

   !
   ! hist(i,0)    = fitness of individual i
   ! hist(i,1:nu) = coordinates of individual i
   !
   real(8), allocatable, dimension(:,:), private :: hist !< History of the populations and their fitness

contains

   !> \brief Initializes the history list
   subroutine initialize_history(ng, np, nu)
      implicit none
      integer, intent(in) :: ng    !< Maximum number of generations
      integer, intent(in) :: np    !< Size of the population
      integer, intent(in) :: nu    !< Dimension of the problem

      allocate( hist(np*ng,0:nu) )

      hist = 0.d0

   end subroutine initialize_history

end module history
