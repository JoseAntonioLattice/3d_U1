module arrays_mod

    use iso_fortran_env, only : dp => real64

    implicit none

    real(dp), allocatable, dimension(:,:,:,:) :: phi
    real(dp), allocatable, dimension(:)       :: temperature



end module arrays_mod
