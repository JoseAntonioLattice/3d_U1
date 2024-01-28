module plaquette_mod

    use precision_mod
    use periodic_boundary_contidions_mod, only : im, ip

contains

    function plaquette_phase_angle_12(phi,ix,iy,iz)

    implicit none
    real(dp) :: plaquette_phase_angle_12
    real(dp), dimension(:,:,:,:), intent(in) :: phi
    integer, intent(in) :: ix, iy, iz

    !Plaquette phase angle in plane 12
    plaquette_phase_angle_12 = - phi(ix,iy,iz,2) - phi(ix,ip(iy),iz,1) + phi(ip(ix),iy,iz,2) + phi(ix,iy,iz,1)


    end function plaquette_phase_angle_12

    function plaquette_phase_angle_13(phi,ix,iy,iz)

    implicit none
    real(dp) :: plaquette_phase_angle_13
    real(dp), dimension(:,:,:,:), intent(in) :: phi
    integer, intent(in) :: ix, iy, iz

    !Plaquette phase angle in plane 13
    plaquette_phase_angle_13 = - phi(ix,iy,iz,3) - phi(ix,iy,ip(iz),1) + phi(ip(ix),iy,iz,3) + phi(ix,iy,iz,1)


    end function plaquette_phase_angle_13

    function plaquette_phase_angle_23(phi,ix,iy,iz)

    implicit none
    real(dp) :: plaquette_phase_angle_23
    real(dp), dimension(:,:,:,:), intent(in) :: phi
    integer, intent(in) :: ix, iy, iz

    !Plaquette phase angle in plane 23
    plaquette_phase_angle_23 = - phi(ix,iy,iz,3) - phi(ix,iy,ip(iz),2) + phi(ix,ip(iy),iz,3) + phi(ix,iy,iz,2)


    end function plaquette_phase_angle_23

end module plaquette_mod
