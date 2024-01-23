program analysis

    use precision_mod
    use statistics
    use number2string_mod
    use get_array

    implicit none

    real(dp) :: temperature
    real(dp), allocatable, dimension(:) :: temp
    real(dp) :: energy_density, sum_eden, sumsq_eden, err_eden
    real(dp) :: monopole_density, sum_monoden, sumsq_monoden, err_monoden

    integer :: L = 8
    integer :: tau_Q = 1024
    integer :: N_measurements = 100

    integer :: i, i_tau, i_temp

    character(*), parameter :: path = '/home/jose/Documents/doctorado/3d_U1_gauge_outeq/data/'
    character(100) :: L_directory
    character(100) :: tau_Q_directory
    character(100) :: temperature_directory
    character(100) :: filepath
    character(100) :: measurementsfile

    L_directory = path//"L="//trim(int2str(L))

    print*, L_directory


    call out_of_equilibrium()


contains

    subroutine equilibrium

        call getvec(temp, trim(path)//'/temperatures.dat')

        open(unit = 666, file = trim(path)//'L='//trim(int2str(L))//'_heatbath.dat')

        do i_temp = 1, size(temp)
            temperature_directory = trim(L_directory)//'/T='//trim(real2str(temp(i_temp)))
            filepath = trim(temperature_directory)//'/history_metropolis.dat'
            !print*, filepath
            open(unit = 100, file = trim(filepath))
            !print*, 'ok'
            sum_eden    = 0.0_dp ; sumsq_eden    = 0.0_dp
            sum_monoden = 0.0_dp ; sumsq_monoden = 0.0_dp
            do i = 1, N_measurements
                !print*,i, 'ok loop'
                read(100,*) energy_density, monopole_density
                sum_eden   = sum_eden   + energy_density
                sumsq_eden = sumsq_eden + energy_density**2

                sum_monoden   = sum_monoden   + monopole_density
                sumsq_monoden = sumsq_monoden + monopole_density**2
            end do
            close(100)
        call std_err(sum_eden, sumsq_eden,N_measurements,energy_density,err_eden)
        call std_err(sum_monoden, sumsq_monoden,N_measurements,monopole_density, err_monoden)

        write(*,*)   temp(i_temp), energy_density,err_eden, monopole_density, err_monoden
        write(666,*) temp(i_temp), energy_density,err_eden, monopole_density, err_monoden

        end do
    end subroutine equilibrium



    subroutine out_of_equilibrium


    tau_Q_directory = trim(L_directory)//"/"//"tau_Q="//trim(int2str(tau_Q))

    print*, tau_Q_directory

    measurementsfile = '/home/jose/Documents/doctorado/3d_U1_gauge_outeq/analysis/L='&
                       //trim(int2str(L))//'_tau_Q='//trim(int2str(tau_Q))//'_heatbath.dat'
    print*, measurementsfile
    open(unit = 666, file = measurementsfile)

    do i_tau = -tau_Q, tau_Q

        filepath = trim(tau_Q_directory)//"/"//'heatbath_t='//trim(int2str(i_tau))//'.dat'
        !print*,filepath
        open(unit = 100, file = trim(filepath))

        read(100,*) temperature

        sum_eden    = 0.0_dp ; sumsq_eden    = 0.0_dp
        sum_monoden = 0.0_dp ; sumsq_monoden = 0.0_dp
        do i = 1, N_measurements
            read(100,*) energy_density, monopole_density
            sum_eden   = sum_eden   + energy_density
            sumsq_eden = sumsq_eden + energy_density**2

            sum_monoden   = sum_monoden   + monopole_density
            sumsq_monoden = sumsq_monoden + monopole_density**2
        end do


        call std_err(sum_eden, sumsq_eden,N_measurements,energy_density,err_eden)
        call std_err(sum_monoden, sumsq_monoden,N_measurements,monopole_density, err_monoden)

        write(*,*) i_tau, energy_density,err_eden, monopole_density, err_monoden
        write(666,*) temperature, energy_density,err_eden, monopole_density, err_monoden
    end do


    end subroutine out_of_equilibrium

end program analysis
