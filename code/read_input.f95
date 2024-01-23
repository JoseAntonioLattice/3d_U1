subroutine read_input()

    use parameters_mod

    implicit none


    Namelist /input_parameters/  L, tau_Q, Ti, N_thermalization, N_measurements, path, algorithm

    open(unit = 666, file = 'input_parameters.par', status = 'old')
    read(unit = 666, nml = input_parameters)
    close(666)



end subroutine read_input
