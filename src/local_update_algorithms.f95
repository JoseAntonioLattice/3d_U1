module local_update_algorithms_mod

    use iso_fortran_env, only : dp => real64, i4 => int32

    implicit none

contains

    subroutine metropolis(U,U_p,deltaS,prob)

    implicit none

    real(dp), intent(inout) :: U
    real(dp), intent(in) :: U_p
    real(dp), intent(in)  :: deltaS
    real(dp), intent(out) :: prob

    real(dp) :: r

        call random_number(r)
        if( deltaS <= 0.0_dp )then
            U = U_p
            prob = 1.0_dp
        elseif( deltaS > 0.0_dp .and. exp( -deltaS ) > r )then
            U = U_p
            prob = exp( -deltaS )
        end if

    end subroutine metropolis


    subroutine GLAUBER(U,U_p,DeltaS,prob)
    implicit none
    real(dp), intent(in) :: DeltaS
    real(dp), intent(inout) :: U
    real(dp), intent(in) :: U_p
    real(dp), intent(out) :: prob
    real(dp) :: r

        call random_number(r)
        prob = EXP(1/(EXP(DeltaS) + 1.0_dp))
        if( prob > R ) THEN
            U = U_p
        endif

    end subroutine GLAUBER

subroutine heatbath(phi,idx,idy,idz,id,beta)

    use periodic_boundary_contidions_mod, only : im
    use maths_mod, only : pi
    use plaquette_mod, only : theta_12 => plaquette_phase_angle_12, &
                              theta_13 => plaquette_phase_angle_13, &
                              theta_23 => plaquette_phase_angle_23
    implicit none

    real(dp), dimension(:,:,:,:), intent(inout) :: phi
    integer, intent(in) :: idx, idy, idz, id
    real(dp), intent(in) :: beta

    real(dp) :: alpha_1, alpha_2, alpha_3, alpha_4
    real(dp) :: v, gamma
    REAL(dp) :: phi_old
    real(dp) :: Z1, Z2, y_max

    integer(i4) :: i

    phi_old = phi(idx,idy,idz,id)

    select case(id)
    case(1)

        alpha_1 = theta_12(phi,idx,idy,idz)     - phi_old
        alpha_2 = theta_12(phi,idx,im(idy),idz) + phi_old
        alpha_3 = theta_13(phi,idx,idy,idz)     - phi_old
        alpha_4 = theta_13(phi,idx,idy,im(idz)) + phi_old

        v     = sqrt( (cos(alpha_1) + cos(alpha_2) + cos(alpha_3) + cos(alpha_4))**2 &
                    + (sin(alpha_1) - sin(alpha_2) + sin(alpha_3) - sin(alpha_4))**2 )

        gamma = atan2( sin(alpha_1) - sin(alpha_2) + sin(alpha_3) - sin(alpha_4), &
                       cos(alpha_1) + cos(alpha_2) + cos(alpha_3) + cos(alpha_4) )

    case(2)

        alpha_1 = theta_12(phi,idx,idy,idz)     + phi_old
        alpha_2 = theta_12(phi,im(idx),idy,idz) - phi_old
        alpha_3 = theta_23(phi,idx,idy,idz)     - phi_old
        alpha_4 = theta_23(phi,idx,idy,im(idz)) + phi_old

        v     = sqrt( ( cos(alpha_1) + cos(alpha_2) + cos(alpha_3) + cos(alpha_4))**2 &
                    + (-sin(alpha_1) + sin(alpha_2) + sin(alpha_3) - sin(alpha_4))**2 )

        gamma = atan2( -sin(alpha_1) + sin(alpha_2) + sin(alpha_3) - sin(alpha_4), &
                        cos(alpha_1) + cos(alpha_2) + cos(alpha_3) + cos(alpha_4))

    case(3)

        alpha_1 = theta_13(phi,idx,idy,idz)     + phi_old
        alpha_2 = theta_13(phi,im(idx),idy,idz) - phi_old
        alpha_3 = theta_23(phi,idx,idy,idz)     + phi_old
        alpha_4 = theta_23(phi,idx,im(idy),idz) - phi_old

        v     = sqrt( ( cos(alpha_1) + cos(alpha_2) + cos(alpha_3) + cos(alpha_4))**2 &
                    + (-sin(alpha_1) + sin(alpha_2) - sin(alpha_3) + sin(alpha_4))**2 )

        gamma = atan2( -sin(alpha_1) + sin(alpha_2) - sin(alpha_3) + sin(alpha_4), &
                        cos(alpha_1) + cos(alpha_2) + cos(alpha_3) + cos(alpha_4))

    case default
        print*, 'Error in heatbath algorithm. Stop.'
        stop
    end select

    if(gamma < 0.0_dp) gamma = gamma + 2.0_dp * pi

    y_max = exp(beta * v)

    call generate_random_numbers(Z1,Z2,y_max)

    i = 0
    do while (exp(beta * v * cos(Z1 + gamma)) < Z2)
        call generate_random_numbers(Z1,Z2,y_max)
        i = i + 1
    end do

    phi(idx,idy,idz,id) = Z1
    !phi(idx,idy,idz,id) = mod(-2*gamma - phi(idx,idy,idz,id) + 6*pi,2*pi)

end subroutine heatbath


    subroutine generate_random_numbers(Z1,Z2,y_max)
    use maths_mod, only : pi
    implicit none

    real(dp), intent(out) :: Z1, Z2
    real(dp), intent(in)  :: y_max

    real(dp) :: U1, U2

        call random_number(U1)
        call random_number(U2)

        Z1 = 2.0_dp*U1*pi
        Z2 = y_max * U2

end subroutine generate_random_numbers


end module local_update_algorithms_mod
