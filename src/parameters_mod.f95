module parameters_mod

    use iso_fortran_env, only : dp => real64, i4 => real32

    implicit none

    integer(i4) :: L        ! Lattice length
    integer(i4) :: tau_Q    ! Inverse cooling rate
    real(dp):: Ti       ! Initial temperature
    integer(i4) :: N_thermalization ! Sweeps of thermalization
    integer(i4) :: N_measurements   ! Number of measurements of each temperature
    character(100) :: path      ! Files path
    integer(i4)        :: algorithm ! Algorithm

end module parameters_mod
