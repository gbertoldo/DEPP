
!> \brief Defines the search strategy DE/rand/1

module mod_class_DE_RAND_1

   use mod_random_generator
   use mod_class_abstract_search_strategy
   use mod_class_ehist
   use mod_class_ifile
   use mod_class_system_variables
   use mod_mpi
   use mod_global_parameters
   use mod_search_tools

   implicit none

   ! Makes everything private, except otherwise stated
   private

   ! Public class defining DE/rand/1 search strategy
   type, public, extends(class_abstract_search_strategy) :: class_DE_RAND_1

      private

      real(8) :: dif !< Differential parameter
      real(8) :: crs !< Crossing over parameter

   contains

      procedure, public, pass :: init
      procedure, public, pass :: get_trial
      procedure, public, pass :: feed_back

   end type

contains

   !> \brief Constructor
   subroutine init(this, sys_var, conf_file_name)
      implicit none
      class(class_DE_RAND_1)                    :: this
      class(class_system_variables), intent(in) :: sys_var
      character(len=*),              intent(in) :: conf_file_name

      ! Inner variables
      type(class_ifile)   :: ifile
      character(str_size) :: CID

      call ifile%init(filename=conf_file_name, field_separator='&')
      call ifile%load()
      call ifile%get_value(CID,"CID")

      if (trim(CID)/="DE/RAND/1") then

         call sys_var%logger%print("class_DE_RAND_1: unexpected CID. Stopping.")

         call mod_mpi_finalize()

      end if

      call ifile%get_value(this%dif,"dif")
      call ifile%get_value(this%crs,"crs")

   end subroutine


   !> \brief Generates a trial individual
   subroutine get_trial(this, ind, ehist, x)
      implicit none
      class(class_DE_RAND_1)                :: this
      integer,                  intent(in)  :: ind   ! Number of the individual of the population
      class(class_ehist),       intent(in)  :: ehist ! Evolution history
      real(8), dimension(:),    intent(out) :: x     ! Trial individual


      ! Inner variables
      integer :: nu
      integer :: np
      integer :: r(3) ! indexes of selected individuals


      ! Detecting nu and np
      nu = size(x)
      np = size(ehist%pop,dim=1)

      ! Choosing three individuals from the population
      call select_individuals(np, ind, r)

      x = ehist%pop(r(3),:) + this%dif*(ehist%pop(r(1),:) - ehist%pop(r(2),:))

      call crossing_over(ind, nu, np, this%crs, ehist%pop, x)

   end subroutine


   subroutine feed_back(this, ind, ehist, fit, ecode)
      implicit none
      class(class_DE_RAND_1)                :: this
      integer,                  intent(in)  :: ind     ! Number of the individual of the population
      class(class_ehist),       intent(in)  :: ehist   ! Evolution history
      real(8),                  intent(in)  :: fit     ! Fitness of the trial individual
      integer,                  intent(in)  :: ecode   ! Error code

   end subroutine

end module
