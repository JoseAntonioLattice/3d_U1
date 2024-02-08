program U1_3d_out_eq

    use arrays_mod
    use parameters_mod
    use memory_mod
    use cooling_mod
    use dynamics_mod
    use periodic_boundary_contidions_mod, only:ip,im, set_periodic_bounds

    implicit none

    call read_input()
    call set_memory()
    call set_periodic_bounds(L)
    temperature = inverse_cooling(Ti,tau_Q)
    call create_files()
    call out_of_equilibrium(phi,temperature,N_thermalization,N_measurements)

    !call equilibrium(phi,N_thermalization,N_measurements,10,temperature)

    deallocate(phi,ip,im,temperature)

end program U1_3d_out_eq
