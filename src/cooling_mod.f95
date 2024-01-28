module cooling_mod

    use precision_mod

    implicit none

    contains


    function linear_cooling(Ti,tau_Q) result(res)

        real(dp), intent(in) :: Ti
        integer, intent(in) :: tau_Q

        real(dp), dimension(-tau_Q:tau_Q) :: res

        integer :: i

            do i = -tau_Q, tau_Q
                res(i) = Ti*(1.0_dp - dble(i)/dble(tau_Q))
            end do

    end function linear_cooling


end module cooling_mod
