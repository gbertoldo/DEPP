!> \brief Contains the global variables and subroutines for reading data
!! from input files.
module input

   use mod_global_parameters
   use mod_class_ifile
   use mod_class_system_variables
   use mod_class_ehist

   implicit none

   ! System
   type(class_system_variables) :: sys_var

   ! Evolution history
   type(class_ehist) :: ehist

   ! Simulation
   integer :: reload    !< upload backup data
   integer :: ind       !< number of the individual
   integer :: ibest     !< index of the best individual in the population
   real(8) :: xfit      !< fitness of the trial individual
   real(8), dimension(:),     allocatable :: x      !< trial individual
   integer :: estatus   !< exit status (0=success; 1=failure)

   ! RSM
   real(8) :: fh        !< Fraction of hybridization
   integer :: rsm_tag   !< Stores the return state of application of DE-RSM


end module input
