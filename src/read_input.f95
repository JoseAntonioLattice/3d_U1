subroutine read_input()

    use iso_fortran_env, only : i4 => real32
    use parameters_mod

    implicit none

    character(100) :: parameters_file
    integer(i4)    :: inunit


    Namelist /input_parameters/  L, tau_Q, Ti, N_thermalization, N_measurements, path, algorithm

    read(*,*) parameters_file

    open(newunit = inunit, file = trim(parameters_file), status = 'old')
    read(inunit, nml = input_parameters)
    close(inunit)

    write(*, nml = input_parameters)



end subroutine read_input
