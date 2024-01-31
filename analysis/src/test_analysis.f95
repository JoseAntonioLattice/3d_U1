program test

  use iso_fortran_env, dp => real64, i4 => int32
  use number2string_mod
  use random
  use statistics

  implicit none

  type observable
      real(dp), allocatable, dimension(:) :: array
      real(dp) :: average
      real(dp) :: error
  end type observable

  integer(i4) :: bins

  character(*), parameter :: filename = 'energy'
  character(100) :: filepath
  character(*), parameter :: ext = '.dat'

  type(observable) :: energy, magnetization
  integer(i4) :: i, counter, n_files, n_data, ci, cf

  integer, allocatable, dimension(:) :: counter_array

  real(dp), allocatable :: obs(:,:)

  logical :: file_exists


  call generate_seed()

  !create some files
  n_files = 5!irand(5,15)
  call create_files(filename,ext,n_files)
  do i = 1,n_files
    n_data = 1000!irand(10,20)
    filepath = trim(filename)//"_"//trim(int2str(i))//trim(ext)
    call write_data(trim(filepath),n_data)
  end do

  !count number of files
  n_files = 0
  do
    n_files = n_files + 1
    filepath = trim(filename)//"_"//trim(int2str(n_files))//ext
    inquire( file = filepath, exist = file_exists)
    if( file_exists .eqv. .false.) exit
  end do
  n_files = n_files - 1


  !Read number of columns in each file
  allocate(counter_array(n_files))
  do i = 1, n_files
    filepath = trim(filename)//"_"//trim(int2str(i))//ext
    counter_array(i) = rows(trim(filepath))
  end do


  counter = sum(counter_array)

  allocate(energy%array(counter),magnetization%array(counter), obs(2,counter))

  obs = transpose(reshape([energy%array,magnetization%array],[counter,2]))

  !OPEN THE FILES AND READ THE CONTENT
  do i = 1, n_files
    filepath = trim(filename)//"_"//trim(int2str(i))//ext
    if (i == 1)then
      ci = 1
      cf = counter_array(i)
    else
      ci = sum(counter_array(1:i-1)) + 1
      cf = sum(counter_array(1:i))
    end if
      !print*, i, ci, cf
      call get_observables(trim(filepath), obs(:,ci:cf))
  end do
  energy%array = obs(1,:)
  magnetization%array = obs(2,:)

  !DO STATISTICS
  call max_jackknife_error(energy%array,energy%average,energy%error,bins)
  print*, 'Max jackknife error',energy%error, 'with',bins, 'bins'
  call std_err(energy%array,energy%average,energy%error)
  print*, "Standard error", energy%average, energy%error

  call max_jackknife_error(magnetization%array,magnetization%average,magnetization%error,bins)
  print*, 'Max jackknife error',magnetization%error, 'with',bins, 'bins'
  call std_err(magnetization%array,magnetization%average,magnetization%error)
  print*, "Standard error", magnetization%average, magnetization%error



  contains



  subroutine get_observables(filename, array)

    character(*), intent(in) :: filename
    real(dp), dimension(:,:), intent(out) :: array

    integer(i4) :: i

      open(unit =  input_unit, file = trim(filename), status = 'unknown', action = 'read')
      do i = 1, size(array(1,:))
        read(input_unit,*) array(:,i)
      end do
      close(input_unit)

  end subroutine get_observables

  function rows(filename) result(n)

      character(*), intent(in) :: filename

      integer(i4) :: n
      integer(i4) :: io

      open(unit =  input_unit, file = trim(filename), status = 'old', action = 'read')
      n = 0
      do
        read(input_unit, *, iostat = io)
        if(io < 0) exit
        n = n + 1
      end do
      close(input_unit)

  end function rows


  subroutine create_files(filename,extension,n)

    use number2string_mod

    implicit none

    character(*), intent(in) :: filename, extension
    integer(i4), intent(in) :: n
    character(100) :: filepath
    integer(i4) :: i

    do i = 1, n
      filepath = trim(filename)//"_"//trim(int2str(i))//trim(extension)
      open(unit = 100, file = trim(filepath))
    end do
    close(100)

  end subroutine create_files

  subroutine write_data(filename,n)

    character(*), intent(in) :: filename
    integer(i4), intent(in) :: n
    integer(i4) :: un
    integer(i4) :: i

    open(newunit = un, file = trim(filename), status = 'old', action = 'write')
    do i = 1, n
      write(un,*) drand(10.0_dp,20.0_dp), drand(80.0_dp,100.0_dp)
    end do
    close(un)

  end subroutine write_data



end program test
