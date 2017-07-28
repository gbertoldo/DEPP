!> \brief Implements subroutines for stopping the iterative process
module stopping_condition_module

   use no_improvement_stopping_condition

   implicit none

   character(len=300) :: convergence_info

contains


   !> Initialize module
   subroutine initialize_stopping_condition_module(GNoAcc)
      implicit none
      integer, intent(in) :: GNoAcc !< Maximum number of generations allowed before stopping if no improvement was found

      call initialize_no_improvement_stopping_condition(GNoAcc)

   end subroutine


   !> Checks if the stopping condition was satisfied
   logical function is_stopping_condition_satisfied(nu, np, ng, g, kpm, detol, xmin, xmax, pop, fit) !< Input (all)
      implicit none
      integer, intent(in) :: nu         !< Dimension of the problem
      integer, intent(in) :: np         !< Size of the population
      integer, intent(in) :: ng         !< Maximum number of generations
      integer, intent(in) :: g          !< Current generation
      integer, intent(in) :: kpm        !< Kind of P-measure (1=dimensional, 2=dimensionless)
      real(8), intent(in) :: detol      !< tolerance for the convergence measure in the DE algorithm
      real(8), intent(in) :: xmin(nu)   !< lower boundary constraints
      real(8), intent(in) :: xmax(nu)   !< higher boundary constraints
      real(8), intent(in) :: pop(np,nu) !< Population
      real(8), intent(in) :: fit(np)    !< Fitness of the population

      ! Inner variables
      logical :: fstag ! Fitness stagnation
      real(8) :: pcm   ! Population convergence measure


      is_stopping_condition_satisfied = .false.

      if ( g < 1) then

         is_stopping_condition_satisfied = .false.

         return

      else

         call get_p_measure(kpm, nu, np, xmin, xmax, pop, pcm)

         call feed_no_improvement_stopping_condition(maxval(fit))

         fstag = is_best_fitness_stagnated()

         if ( (pcm <= detol) .or. (ng <= g) .or. fstag ) is_stopping_condition_satisfied = .true.


         write(convergence_info, "(1PE23.15,A,L5,A)")   pcm,  " = Population convergence measure." &
                                                     , fstag,  " = Is fitness stagnated?"

      end if

   end function



   !> \brief Calculates the P-measure, i. e. the population
   !! convergence measure of FEOKTISTOV "Differential evolution" 2006
   subroutine get_p_measure(kpm, nu, np, xmin, xmax, pop, pm)
      implicit none
      integer, intent(in)  :: kpm        !< Kind of P-measure (1=dimensional, 2=dimensionless)
      integer, intent(in)  :: nu         !< Dimension of the problem
      integer, intent(in)  :: np         !< Size of the population
      real(8), intent(in)  :: xmin(nu)   !< lower boundary constraints
      real(8), intent(in)  :: xmax(nu)   !< higher boundary constraints
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

         raux = norma2( apop(j,:) - pb, nu )

         if ( raux > pm ) pm = raux

      end do

   end subroutine get_p_measure




   !> \brief Calculates the norm 2 of a vector
   function norma2(vec, n)
      implicit none
      integer :: n      !< dimension of the vector
      real(8) :: norma2 !< norm of the vector
      real(8) :: vec(n) !< vector

      ! Inner variables

      integer :: i
      real(8) :: soma

      soma = 0.d0

      do i = 1, n

         soma = soma + vec(i)**2

      end do

      norma2 = dsqrt(soma)

      return

   end function norma2

end module
