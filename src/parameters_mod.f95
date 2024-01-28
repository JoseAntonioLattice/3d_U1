module parameters_mod

    use precision_mod

    implicit none

    integer :: L        ! Lattice length
    integer :: tau_Q    ! Inverse cooling rate
    real(dp):: Ti       ! Initial temperature
    integer :: N_thermalization ! Sweeps of thermalization
    integer :: N_measurements   ! Number of measurements of each temperature
    character(100) :: path      ! Files path
    integer        :: algorithm ! Algorithm

end module parameters_mod
