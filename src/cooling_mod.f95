module cooling_mod

    use iso_fortran_env, only : dp => real64, i4 => int32

    implicit none

    contains


    function linear_cooling(Ti,tau_Q) result(res)

        real(dp), intent(in) :: Ti
        integer(i4), intent(in) :: tau_Q

        real(dp), dimension(-tau_Q:tau_Q) :: res
        integer(i4) :: i

            do i = -tau_Q, tau_Q
              res(i) = Ti*(1.0_dp - i/real(tau_Q,dp))
            end do

    end function linear_cooling


end module cooling_mod
