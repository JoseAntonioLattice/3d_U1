program test

  use iso_fortran_env, dp => real64, i4 => int32

  use number2string_mod
  implicit none

  type observable
      real(dp), allocatable, dimension(:) :: array
      real(dp) :: average
      real(dp) :: error
  end type observable

  integer(i4), parameter :: bins = 10

  character(100) :: filepath

  type(observable) :: energy, magnetization
  integer(i4) :: i, j, counter

  real(dp), allocatable :: obs(:,:)
  real(dp) :: r,s

  logical :: file_exists

  !create some files
  do i = 1, 10
    write(filepath,*) "energy_"//trim(int2str(i))//".dat"
    open(unit = input_unit, file = trim(filepath))
    do j = 1, 10
      call random_number(r)
      write(input_unit,*) r, 3*r-1.0_dp
    end do
    close(input_unit)
  end do


  !COUNT HOW MANY ROWS HAS THE FILE
  !counter = rows(filepath)
  counter = 0
  i = 0
  do
    i = i + 1
    write(filepath,*) "energy_"//trim(int2str(i))//".dat"
    inquire( file = trim(filepath), exist = file_exists)
    if( file_exists .eqv. .false.) exit
    counter = counter + rows(trim(filepath))
  end do
  i = i - 1


  print*, i, counter

  allocate(energy%array(counter),magnetization%array(counter), obs(2,counter))

  obs = transpose(reshape([energy%array,magnetization%array],[counter,2]))

  !OPEN THE FILE AND READ THE CONTENT

  !call get_observables("energy_"//trim(int2str(i))//".dat", obs(:,i:10*i))

  !energy%array = obs(1,:)
  !magnetization%array = obs(2,:)

  !print*,energy%array
  !print*, magnetization%array

  !DO STATISTICS
  !call std_err(energy%array,energy%average,energy%error)
  !print*, "Standard error", energy%average, energy%error
  !call jackknife(magnetization%array,magnetization%average,magnetization%error,bins)
  !print*, "Jackknife", magnetization%average, magnetization%error

  contains

  subroutine std_err(array, average, error)

    implicit none
    real(dp), intent(in), dimension(:) :: array
    real(dp), intent(out) :: average, error

    real(dp) :: variance2
    integer(i4) :: n

      n = size(array)

      average = sum(array)/n
      variance2 = (sum(array**2) - n*average**2)/(n-1)
      error = sqrt(variance2/n)

  end subroutine std_err


  subroutine jackknife(array,average,error,bins)

    real(dp), intent(in), dimension(:) :: array
    real(dp), intent(out) :: average, error
    integer(i4), intent(in) :: bins

    real(dp), dimension(bins) :: theta
    integer(i4) :: n,m
    integer(i4) :: i

      n = size(array)
      m = n/bins

      theta = [(sum(array) - sum(array(m*(i-1)+1:i*m)), i = 1, bins )]/(n-m)
      average = sum(array)/n
      error = sqrt((bins-1)/real(bins,dp)*sum((theta-average)**2))

  end subroutine jackknife


  subroutine get_observables(filename, array)

    character(*), intent(in) :: filename
    real(dp), dimension(:,:), intent(out) :: array

    integer(i4) :: i

      open(unit =  input_unit, file = filename, status = 'old', action = 'read')
      do i = 1, size(array(1,:))
        read(input_unit,*) array(:,i)
      end do
      close(input_unit)

  end subroutine get_observables

  function rows(filename) result(n)

      character(*), intent(in) :: filename

      integer(i4) :: n
      integer(i4) :: io

      open(unit =  input_unit, file = filename, status = 'old', action = 'read')
      n = 0
      do
        read(input_unit, *, iostat = io)
        if(io < 0) exit
        n = n + 1
      end do
      close(input_unit)

  end function rows


end program test
