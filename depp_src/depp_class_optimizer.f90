!    DEPP - Differential Evolution Parallel Program
!
!    Copyright (C) 2019 by Jonas Joacir Radtke, Guilherme Bertoldo and Carlos Henrique Marchi
!
!    This program is free software: you can redistribute it and/or modify
!    it under the terms of the GNU General Public License as published by
!    the Free Software Foundation, either version 3 of the License, or
!    (at your option) any later version.
!
!    This program is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU General Public License for more details.
!
!    You should have received a copy of the GNU General Public License
!    along with this program.  If not, see <http://www.gnu.org/licenses/>.
!    
!    Contact:
!          Jonas Joacir Radtke (a)
!                 E-mail: jonas.radtke@gmail.com
!
!          Guilherme Bertoldo (a)
!                 E-mail: glbertoldo@gmail.com
!
!          Carlos Henrique Marchi (b)
!                 E-mail: chmcfd@gmail.com
!    Institution
!          (a) Federal University of Technology - Paraná - UTFPR
!              Linha Santa Bárbara, s/n, Francisco Beltrão, Paraná, Brazil
!              Zip Code 85601-970
!              
!          (b) Federal University of Paraná - UFPR
!              Curitiba, Paraná, Brazil
!              Caixa postal 19040
!              Zip Code 81531-980
!

!> \brief This module contains the optimization algorithm

module mod_class_optimizer

   use mod_mpi
   use mod_random_generator
   use mod_string
   use mod_class_btimer
   use mod_class_system_variables
   use mod_class_ehist
   use mod_class_composite_stop_condition
   use mod_class_parallel_processed_trial_population

   implicit none

   ! Makes everything private, except otherwise stated
   private

   !> \brief Class optimizer
   type, public :: class_optimizer

      private

      type(class_system_variables)                    :: sys_var  !< System variables
      type(class_ehist)                               :: ehist    !< Evolution history
      type(class_btimer)                              :: timer    !< Timer
      type(class_composite_stop_condition)            :: stopper  !< Stop condition object
      type(class_parallel_processed_trial_population) :: searcher !< Searcher object

   contains

      procedure, public, pass :: init !< Constructor
      procedure, public, pass :: run  !< Performs optimization

   end type


contains


   !> \brief Constructor
   subroutine init(this)
      implicit none
      class(class_optimizer) :: this !< A reference to this object


      ! Creating labels
      associate (                      &
            sys_var  => this%sys_var,  &
            timer    => this%timer,    &
            ehist    => this%ehist,    &
            searcher => this%searcher, &
            stopper  => this%stopper   )


         ! Initializing MPI module
         call mod_mpi_init()


         ! Initializing random number generator module
         call initialize_random_generator()


         ! Initializing system variables
         call sys_var%init()


         ! Initializing evolution history
         call ehist%init(sys_var)


         ! Initializing trial individuals searcher
         call searcher%init(sys_var, ehist)


         ! Initializing stop condition object
         call stopper%init(sys_var)


         ! Initializing timer
         call timer%init(sys_var)


      end associate


   end subroutine



   !> \brief Optimization algorithm
   subroutine run(this)
      implicit none
      class(class_optimizer) :: this !< A reference to this object


      ! Creating labels
      associate (                      &
            sys_var  => this%sys_var,  &
            timer    => this%timer,    &
            ehist    => this%ehist,    &
            searcher => this%searcher, &
            stopper  => this%stopper   )



         ! Processors synchronization
         call mod_mpi_barrier()


         ! Starting the generations loop. This loop is maintained while the stopping
         ! condition is not satisfied
         do

            ! Computing stop condition
            call stopper%compute_stop_condition(ehist)


            ! Printing convergence measure of the current generation
            call sys_var%logger%print(stopper%convergence_info())


            ! Checking stop condition
            if ( stopper%is_stop_condition_satisfied() ) exit


            ! Starting a new generation
            call ehist%new_generation()


            ! Printing CPU time
            call timer%measure()

            call sys_var%logger%println("Accumulated CPU time: " // timer%formatted_elapsed_time())


            ! Generates a trial population and calculates its fitness function
            ! based on the evolution history (ehist). The trial population is added
            ! to the evolution history.
            call searcher%get_trial_population()


            ! Selects best individuals among trial population and current population
            call ehist%select_individuals()


            ! Printing evolution history information
            call sys_var%logger%print(ehist%info())

            ! Calculating the ellapsed CPU time
            call timer%measure()

            ! Saving backup of cpu time
            call timer%save_backup()

            ! Saving backup data
            call ehist%save_backup(sys_var)

            call mod_mpi_barrier()

         end do


         ! Printing final solution
         call sys_var%logger%println(ehist%final_solution_info())
         call sys_var%logger%println(stopper%final_convergence_info())
         call sys_var%logger%println(to_string(timer%elapsed_time()) // " : Accumulated CPU time (s)")
         call sys_var%logger%println(timer%formatted_elapsed_time() // " : Accumulated CPU time (h:mm:ss)")


         ! Finishing MPI
         call mod_mpi_finalize()


      end associate


   end subroutine


end module
