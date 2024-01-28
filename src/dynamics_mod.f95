module dynamics_mod

    use precision_mod
    implicit none

    private
    public out_of_equilibrium, equilibrium

    contains

    subroutine thermalization(phi,N_thermalization, temperature)

        real(dp), dimension(:,:,:,:), intent(inout) :: phi
        integer, intent(in) :: N_thermalization
        real(dp) :: temperature

        integer :: i_sweeps

            if(temperature > 0.5_dp)then
                call hot_start(phi)
            elseif(temperature >= 0.0_dp .and. temperature <= 0.5_dp)then
                call cold_start(phi)
            elseif(temperature < 0.0_dp)then
                print*, 'ERROR in "thermalization" subroutine. Negative temperature. STOP'
                stop
            end if

            do i_sweeps = 1, N_thermalization
                call sweeps(phi,temperature)
            end do

    end subroutine thermalization

    subroutine hot_start(phi)

        use maths_mod, only : pi

        real(dp), dimension(:,:,:,:), intent(inout) :: phi

            call random_number(phi)
            phi = 2.0_dp * pi * phi

    end subroutine hot_start


    subroutine cold_start(phi)

        real(dp), dimension(:,:,:,:), intent(inout) :: phi

            phi = 0.0_dp
    end subroutine cold_start

    subroutine out_of_equilibrium(phi,temperature,N_thermalization,N_measurements)

        use parameters_mod, only : L, tau_Q, path, algorithm
        use number2string_mod
        use observables_mod

        real(dp), dimension(:,:,:,:), intent(inout) :: phi
        real(dp), dimension(-tau_Q:tau_Q), intent(in) :: temperature
        integer, intent(in) :: N_measurements, N_thermalization

        character(100) :: L_directory, tau_Q_directory, filepath
        character(20)  :: algorithm_id

        integer :: i_tau,i

        L_directory = trim(path)//"L="//trim(int2str(L))
        tau_Q_directory = trim(L_directory)//"/"//"tau_Q="//trim(int2str(tau_Q))


        select case(algorithm)
        case(1)
            algorithm_id = 'metropolis'
        case(2)
            algorithm_id = 'glauber'
        case(3)
            algorithm_id = 'heatbath'
        case default
            print*, 'ERROR in "out_of_equilibrium" subroutine. Not a valid algorithm. STOP.'
            stop
        end select


        do i = 1, N_measurements
            call thermalization(phi,N_thermalization,temperature(-tau_Q))
            do i_tau = -tau_Q, tau_Q
                call sweeps(phi,temperature(i_tau))
                filepath = trim(tau_Q_directory)//"/"//trim(algorithm_id)//"_t="//trim(int2str(i_tau))//".dat"
                call take_measurements(phi)
                open( unit = 100, file = trim(filepath), status = 'old', position = 'append' )
                write(100,*) energy_density, monopole_density
                close(100)
            end do
        end do

    end subroutine out_of_equilibrium

    subroutine sweeps(phi,temperature)

        use parameters_mod, only : L, algorithm
        use maths_mod, only : pi
        use local_update_algorithms_mod

        real(dp), dimension(:,:,:,:), intent(inout) :: phi
        real(dp), intent(in) :: temperature

        integer  :: ix, iy, iz, id
        real(dp) :: Delta_S
        real(dp) :: phi_p
        real(dp) :: beta
        real(dp) :: probability

        if(temperature == 0.0_dp) beta = 10.0_dp**10
        if(temperature /= 0.0_dp) beta = 1/temperature

        do ix = 1, L
            do iy = 1, L
                do iz = 1, L
                    do id = 1, 3
                        select case(algorithm)
                        case(1)
                           call random_number(phi_p); phi_p = 2.0_dp*pi*phi_p
                           Delta_S = beta * Delta_E(phi,phi_p,ix,iy,iz,id)
                           call metropolis(phi(ix,iy,iz,id),phi_p,Delta_S,probability)
                        case(2)
                           call random_number(phi_p); phi_p = 2.0_dp*pi*phi_p
                           Delta_S = beta * Delta_E(phi,phi_p,ix,iy,iz,id)
                           call glauber(phi(ix,iy,iz,id),phi_p,Delta_S,probability)
                        case(3)
                           call heatbath(phi,ix,iy,iz,id,beta)
                        case default
                           print*, 'ERROR in the "sweep" subroutine. Not a valid algorithm. STOP.'
                           stop
                        end select
                    end do
                end do
            end do
        end do
    end subroutine sweeps



    subroutine take_measurements(phi)


        use maths_mod, only : pi
        use parameters_mod, only : L
        use observables_mod
        use plaquette_mod
        use periodic_boundary_contidions_mod, only : ip

        real(dp), dimension(:,:,:,:), intent(in) :: phi

        integer :: ix, iy, iz
        real(dp) :: W1, W2, W3, W4, W5, W6

        Energy_density   = 0.0_dp
        monopole_density = 0.0_dp

        do ix = 1, L
            do iy = 1, L
                do iz = 1, L
                    W1 = mod(plaquette_phase_angle_12(phi,ix,iy,iz)     + 4*pi,2*pi); if(W1 > pi) W1 = W1 - 2*pi
                    W2 = mod(plaquette_phase_angle_13(phi,ix,iy,iz)     + 4*pi,2*pi); if(W2 > pi) W2 = W2 - 2*pi
                    W3 = mod(plaquette_phase_angle_23(phi,ix,iy,iz)     + 4*pi,2*pi); if(W3 > pi) W3 = W3 - 2*pi
                    W4 = mod(plaquette_phase_angle_23(phi,ip(ix),iy,iz) + 4*pi,2*pi); if(W4 > pi) W4 = W4 - 2*pi
                    W5 = mod(plaquette_phase_angle_13(phi,ix,ip(iy),iz) + 4*pi,2*pi); if(W5 > pi) W5 = W5 - 2*pi
                    W6 = mod(plaquette_phase_angle_12(phi,ix,iy,ip(iz)) + 4*pi,2*pi); if(W6 > pi) W6 = W6 - 2*pi
                    Energy_density   = Energy_density   + cos(W1) + cos(W2) + cos(W3)
                    monopole_density = monopole_density + abs( -W1 + W2 - W3 + W4 - W5 + W6 )
                end do
            end do
        end do

        Energy_density   = 3.0_dp - Energy_density/dble(L**3)
        monopole_density = monopole_density/dble(2*pi*L**3)


    end subroutine take_measurements


    subroutine equilibrium(phi, N_thermalization, N_measurements, N_skip, temperature)

        use check_files_directories_mod
        use parameters_mod, only : L, path
        use number2string_mod
        use observables_mod

        real(dp), dimension(:,:,:,:), intent(inout) :: phi
        integer, intent(in) :: N_thermalization, N_measurements, N_skip
        real(dp), dimension(:), intent(in) :: temperature

        character(100):: L_directory, temperature_directory, filepath
        integer :: i_temp,i

        L_directory = trim(path)//"L="//trim(int2str(L))


        do i_temp = 1, size(temperature)
            temperature_directory = trim(L_directory)//'/T='//trim(real2str(temperature(i_temp)))
            call thermalization(phi,N_thermalization,temperature(i_temp))
            call check_directory(trim(temperature_directory))
            open(unit = 100, file = trim(temperature_directory)//'/history_metropolis.dat', status = 'new')
            do i = 1, N_measurements
                call sweeps(phi,temperature(i_temp))
                if(mod(i,n_skip) == 0)then
                    call take_measurements(phi)
                    write(100,*) energy_density, monopole_density
                end if
            end do
            close(100)
        end do


    end subroutine equilibrium


    function Delta_E(phi,phi_p,ix,iy,iz,id) result(res)

        use plaquette_mod

        real(dp) :: res

        real(dp), dimension(:,:,:,:), intent(inout) :: phi
        real(dp), intent(in) :: phi_p
        integer, intent(in)  :: ix,iy,iz,id

        real(dp) :: phi_old

        real(dp) :: energy_new, energy_old


        phi_old = phi(ix,iy,iz,id)

        select case(id)
        case(1)
            energy_old = - ( cos(plaquette_phase_angle_12(phi,ix,im(iy),iz)) + cos(plaquette_phase_angle_12(phi,ix,iy,iz))  &
                           + cos(plaquette_phase_angle_13(phi,ix,iy,im(iz))) + cos(plaquette_phase_angle_13(phi,ix,iy,iz)) )

            phi(ix,iy,iz,id) = phi_p

            energy_new = - ( cos(plaquette_phase_angle_12(phi,ix,im(iy),iz)) + cos(plaquette_phase_angle_12(phi,ix,iy,iz))  &
                           + cos(plaquette_phase_angle_13(phi,ix,iy,im(iz))) + cos(plaquette_phase_angle_13(phi,ix,iy,iz)) )

            phi(ix,iy,iz,id) = phi_old
        case(2)

            energy_old = - ( cos(plaquette_phase_angle_12(phi,im(ix),iy,iz)) + cos(plaquette_phase_angle_12(phi,ix,iy,iz)) &
                           + cos(plaquette_phase_angle_23(phi,ix,iy,im(iz))) + cos(plaquette_phase_angle_23(phi,ix,iy,iz)) )

            phi(ix,iy,iz,id) = phi_p

            energy_new = - ( cos(plaquette_phase_angle_12(phi,im(ix),iy,iz)) + cos(plaquette_phase_angle_12(phi,ix,iy,iz)) &
                           + cos(plaquette_phase_angle_23(phi,ix,iy,im(iz))) + cos(plaquette_phase_angle_23(phi,ix,iy,iz)) )

            phi(ix,iy,iz,id) = phi_old

        case(3)

            energy_old = - ( cos(plaquette_phase_angle_13(phi,im(ix),iy,iz)) + cos(plaquette_phase_angle_13(phi,ix,iy,iz)) &
                           + cos(plaquette_phase_angle_23(phi,ix,im(iy),iz)) + cos(plaquette_phase_angle_23(phi,ix,iy,iz)) )

            phi(ix,iy,iz,id) = phi_p

            energy_new = - ( cos(plaquette_phase_angle_13(phi,im(ix),iy,iz)) + cos(plaquette_phase_angle_13(phi,ix,iy,iz)) &
                           + cos(plaquette_phase_angle_23(phi,ix,im(iy),iz)) + cos(plaquette_phase_angle_23(phi,ix,iy,iz)) )

            phi(ix,iy,iz,id) = phi_old
        case default
            print*, 'ERROR in the "Delta_E" function. Not a valid direction. STOP.'
            stop
        end select

        res = energy_new - energy_old

    end function Delta_E

end module dynamics_mod
