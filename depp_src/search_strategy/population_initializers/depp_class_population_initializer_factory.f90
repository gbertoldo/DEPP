
!> \brief Provides a factory for population initializers classes

module mod_class_population_initializer_factory

   use mod_mpi
   use mod_string
   use mod_class_ifile
   use mod_class_ehist
   use mod_class_system_variables
   use mod_class_abstract_population_initializer
   use mod_class_population_initializer_uniform_random

   implicit none

   ! Makes everything private, except otherwise stated
   private

   !> \brief A factory for population initializers classes
   type, public :: class_population_initializer_factory

   contains

      procedure, pass, public :: create !< Creates a population initializer object

   end type

contains

   !> \brief Creates a population initializer object
   subroutine create(this, sys_var, conf_file_name, population_initializer)
      implicit none
      class(class_population_initializer_factory)                        :: this                   !< A reference to this object
      class(class_system_variables),                         intent(in)  :: sys_var                !< System's variables
      character(len=*),                                      intent(in)  :: conf_file_name         !< Configuration file
      class(class_abstract_population_initializer), pointer, intent(out) :: population_initializer !< Population initializer object

      ! Inner variables
      type(class_ifile)       :: ifile
      character(len=str_size) :: model

      ! Reading population initializer model
      call ifile%init(filename=sys_var%absparfile, field_separator='&')

      call ifile%load()

      call ifile%get_value(model,"pop_initializer_model")


      ! Allocating object

      if ( .not. associated(population_initializer) ) then

         if ( trim(model) == "uniform_random" ) then

            allocate(class_population_initializer_uniform_random::population_initializer)

         else

            call sys_var%logger%println("class_population_initializer_factory: Unkown model. Stopping.")

            call mod_mpi_finalize()

         end if


         ! Initializing the object
         select type (population_initializer)

            type is ( class_population_initializer_uniform_random )

         end select

      else

         call sys_var%logger%println("class_population_initializer_factory: Pointer already associated. Stopping.")

         call mod_mpi_finalize()

      end if

   end subroutine


end module
