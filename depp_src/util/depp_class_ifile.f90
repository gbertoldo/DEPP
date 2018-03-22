!>
!!
!! \brief Defines a class for reading variables values, names and
!! descriptions from input files. Input file must provide data
!! according to the following pattern:
!!
!!  VALUE   FS  VARIABLE NAME  FS DESCRIPTION
!!
!! where FS is a field separator.
!!

module mod_class_ifile

   use mod_mpi

   implicit none


  ! Makes everything private, except otherwise stated
   private

   !! Maximum character length
   integer, parameter, private :: chlen = 500


   !> Defines a basic structure of input data
   !! It is expected that input files contains
   !! data in the following pattern:
   !!
   !! value FS varname FS description
   !!
   !! where FS is the field separator char
   type, private :: basic_struct

      type(character(chlen)) :: value        !< Value
      type(character(chlen)) :: varname      !< Variable name or ID
      type(character(chlen)) :: description  !< Description

   end type


   !> Defines the ifile class
   type, public :: class_ifile

      character(len=10), private                             :: field_separator !< Field separator
      type(character(chlen)), private                        :: filename        !< File name
      type(basic_struct), dimension(:), allocatable, private :: data            !< List of data
      integer,            dimension(:), allocatable, private :: pickedup        !< List of picked up data

   contains

      procedure, public,  pass :: init                             !< Constructor
      procedure, public,  pass :: clear                            !< Clears allocated memory
      procedure, public,  pass :: load                             !< Loads input file data to ifile object
      generic,   public        :: get_value => get_int_value    &  !< Gets the value of 'varname' (overload function)
         ,                                     get_real8_value  &
         ,                                     get_string_value
      procedure, private, pass :: get_int_value                    !< Get integer value
      procedure, private, pass :: get_real8_value                  !< Get double value
      procedure, private, pass :: get_string_value                 !< Get string value
      procedure, public,  pass :: print_read_data                  !< Print read data
      procedure, public,  pass :: print_pickedup_data              !< Print picked up data

   end type


contains


   !> \brief Initializes ifile object
   subroutine init(this, filename, field_separator)
      implicit none
      class(class_ifile)                 :: this            !< A reference to this object
      type(character(len=*)), intent(in) :: filename        !< File name
      type(character(len=*)), intent(in) :: field_separator !< Field separator

      call this%clear()

      this%filename = filename

      this%field_separator = field_separator

   end subroutine


   !> \brief Clears allocated memory
   subroutine clear(this)
      implicit none
      class(class_ifile) :: this !< A reference to this object

      if (allocated(this%data))     deallocate(this%data)
      if (allocated(this%pickedup)) deallocate(this%pickedup)

   end subroutine


   !> \brief Loads input file data to ifile object
   subroutine load(this)
      implicit none
      class(class_ifile) :: this !< A reference to this object

      ! Inner variables

      integer :: IO
      integer :: counter
      logical :: last_token
      character(len=3*chlen) :: line
      character(len=3*chlen), dimension(3) :: tokens

      ! Cleaning memory

      call this%clear()

      ! Reading data file

      open(200,file=this%filename)

      do

         read(200,"(A)",IOStat=IO) line

         if (IO/=0) exit

         ! Parsing line

         counter = 0

         do

            counter = counter + 1

            tokens(counter) = next_token(line,trim(this%field_separator),last_token)

            ! If it was read 2 tokens and there is at least one more token, then
            ! the third token will be equal to the remainder line, independently
            ! of the number of remainder tokens. This is necessary to avoid truncation
            ! in the 'description' field.
            if ( .not. last_token .and. counter == 2) then

               counter = counter + 1

               tokens(counter) = trim(adjustl(line))

               exit

            end if

            if (last_token .or. counter == 3) exit

         end do

         if ( 3 == counter ) then

            call add_data(this, value=tokens(1), varname=tokens(2), description=tokens(3))

         end if

      end do

      close(200)

      ! Checks for variables defined repeatedly
      call check_var_repetition(this)

   end subroutine


   !> Checks for variables repeated in the input file
   subroutine check_var_repetition(this)
      implicit none
      class(class_ifile) :: this !< A reference to this object

      ! Inner variables

      integer :: i
      integer :: j

      do i = 1, size(this%data)

         do j = i+1, size(this%data)

            if (trim(adjustl(this%data(i)%varname))==trim(adjustl(this%data(j)%varname))) then

               write(*,*) trim(adjustl(this%filename)), ": '", trim(adjustl(this%data(j)%varname)), "' was defined repeatedly."

            end if

         end do

      end do

   end subroutine


   !> \brief Add data to the end of the list
   subroutine add_data(this, value, varname, description)
      implicit none
      class(class_ifile)           :: this        !< A reference to this object
      character(len=*), intent(in) :: value       !< Value of varname
      character(len=*), intent(in) :: varname     !< Variable name
      character(len=*), intent(in) :: description !< Variable description

      integer :: Nb
      type(basic_struct), dimension(:), allocatable :: tmp

      Nb = 0

      if (allocated(this%data)) Nb = size(this%data)

      allocate (tmp(1:Nb+1))

      if ( allocated (this%data) ) tmp(1:Nb) = this%data

      ! MOVE_ALLOC(FROM, TO) moves the allocation from FROM to TO. FROM will become deallocated in the process.
      call move_alloc(tmp, this%data)

      this%data(Nb+1)%value       = trim(value)
      this%data(Nb+1)%varname     = trim(varname)
      this%data(Nb+1)%description = trim(description)

   end subroutine


   !> \brief Finds the next token in str separated by FS
   function next_token(str,FS,last_token) result(token)
      implicit none
      character(len=*),        intent(inout) :: str         !< Input string
      character(len=*),        intent(in)    :: FS          !< Set of field separators
      character(len=len(str))                :: token       !< Next token found
      logical,                 intent(out)   :: last_token  !< Returns true if this is the last_token token


      ! Inner variables
      integer :: idx

      last_token = .false.

      idx = scan(str,trim(FS))


      if ( 0 < idx ) then

         if ( idx == 1 ) then

            token = ""

         else

            token = str(1:idx-1)

        end if

         if ( idx < len(str) ) then

            str = str(idx+1:)

            last_token = .false.

         else

            str = ""

            last_token = .true.

         end if

      else

         token = str

         str = ""

         last_token = .true.

      end if

   end function


   !> \brief Prints all data read from file
   subroutine print_read_data(this,unt,frm)
      implicit none
      class(class_ifile)                     :: this !< A reference to this object
      integer, intent(in)                    :: unt  !< Unit to print
      character(len=*), optional, intent(in) :: frm  !< Output format

      ! Inner variables
      integer :: i

      do i=1, size(this%data)

         if ( present(frm) ) then

            write(unt,frm) trim(adjustl(this%data(i)%value))      &
               ,           trim(adjustl(this%data(i)%varname))    &
               ,           trim(adjustl(this%data(i)%description))

         else

            write(unt,"(2(1X,A30),2X,A)") trim(adjustl(this%data(i)%value))   // " &" &
               ,                          trim(adjustl(this%data(i)%varname)) // " &" &
               ,                          trim(adjustl(this%data(i)%description))

         end if

      end do

   end subroutine


   !> \brief Returns the value of the int corresponding to varname
   subroutine get_int_value(this,value,varname)
      implicit none
      class(class_ifile)            :: this    !< A reference to this object
      integer,          intent(out) :: value   !< Value of varname
      character(len=*), intent(in)  :: varname !< Variable name

      ! Inner variables
      integer :: i

      value = 0

      do i = 1, size(this%data)

         if (trim(adjustl(varname))==trim(adjustl(this%data(i)%varname))) then

            read(this%data(i)%value,*) value

            call add_pickedup(this, i)

            return

         end if

      end do

      write(*,*) "Variable ", trim(adjustl(varname)), " not found. Stopping."

      call mod_mpi_finalize()

   end subroutine


   !> \brief Returns the value of the real(8) corresponding to varname
   subroutine get_real8_value(this,value,varname)
      implicit none
      class(class_ifile)            :: this    !< A reference to this object
      real(8),          intent(out) :: value   !< Value of varname
      character(len=*), intent(in)  :: varname !< Variable name

      ! Inner variables
      integer :: i

      value = 0.d0

      do i = 1, size(this%data)

         if (trim(adjustl(varname))==trim(adjustl(this%data(i)%varname))) then

            read(this%data(i)%value,*) value

            call add_pickedup(this, i)

            return

         end if

      end do

      write(*,*) "Variable ", trim(adjustl(varname)), " not found. Stopping."

      call mod_mpi_finalize()

   end subroutine



   !> \brief Returns the value of the string corresponding to varname
   subroutine get_string_value(this,value,varname)
      implicit none
      class(class_ifile)            :: this    !< A reference to this object
      character(len=*), intent(out) :: value   !< Value of varname
      character(len=*), intent(in)  :: varname !< Variable name

      ! Inner variables
      integer :: i
      character(len=chlen) :: caux

      value = ""

      do i = 1, size(this%data)

         if (trim(adjustl(varname))==trim(adjustl(this%data(i)%varname))) then

            read(this%data(i)%value,"(A)") caux

            value = trim(adjustl(caux))

            call add_pickedup(this, i)

            return

         end if

      end do

      write(*,*) "Variable ", trim(adjustl(varname)), " not found. Stopping."

      call mod_mpi_finalize()

   end subroutine



   !> \brief Add picked up item to the list
   subroutine add_pickedup(this, idx)
      implicit none
      class(class_ifile)  :: this !< A reference to this object
      integer, intent(in) :: idx  !< Index of the picked data


      integer :: Nb
      integer, dimension(:), allocatable :: tmp

      Nb = 0

      if (allocated(this%pickedup)) Nb = size(this%pickedup)

      allocate (tmp(1:Nb+1))

      if ( allocated (this%pickedup) ) tmp(1:Nb) = this%pickedup

      ! MOVE_ALLOC(FROM, TO) moves the allocation from FROM to TO. FROM will become deallocated in the process.
      call move_alloc(tmp, this%pickedup)

      this%pickedup(Nb+1) = idx

   end subroutine


   !> \brief Prints all data picked up from file
   subroutine print_pickedup_data(this,unt,frm)
      implicit none
      class(class_ifile)                     :: this !< A reference to this object
      integer,                    intent(in) :: unt  !< Unit to print
      character(len=*), optional, intent(in) :: frm  !< Output format

      ! Inner variables
      integer :: i, j

      do j=1, size(this%pickedup)

         i = this%pickedup(j)

         if ( present(frm) ) then

            write(unt,frm) trim(adjustl(this%data(i)%value))      &
               ,           trim(adjustl(this%data(i)%varname))    &
               ,           trim(adjustl(this%data(i)%description))

         else

            write(unt,"(2(1X,A30),2X,A)") trim(adjustl(this%data(i)%value))   // " &" &
               ,                          trim(adjustl(this%data(i)%varname)) // " &" &
               ,                          trim(adjustl(this%data(i)%description))

         end if

      end do

   end subroutine


end module
