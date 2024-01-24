subroutine read_input()

    use parameters_mod

    implicit none

    character(100) :: parameters_file

    Namelist /input_parameters/  L, tau_Q, Ti, N_thermalization, N_measurements, path, algorithm

    read(*,*) parameters_file

    open(unit = 666, file = parameters_file, status = 'old')
    read(unit = 666, nml = input_parameters)
    close(666)

    write(*, nml = input_parameters)



end subroutine read_input
