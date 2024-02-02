subroutine create_files()
    use parameters_mod
    use number2string_mod
    use check_files_directories_mod
    use arrays_mod, only : temperature

    use iso_fortran_env, only: i4 => real32

    implicit none

    integer(i4) :: i
    logical :: file_exists
    integer(i4) :: io, inunit


    character(100) :: L_directory
    character(100) :: tau_Q_directory
    character(100) :: filepath
    character(20)  :: algorithm_id

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
            print*, 'Not valid algorithm. Exiting.'
            stop
    end select


    call check_directory(trim(L_directory))
    call check_directory(trim(tau_Q_directory))

    do i = -tau_Q, tau_Q
        filepath = trim(tau_Q_directory)//"/"//trim(algorithm_id)//"_t="//trim(int2str(i))//".dat"
        call check_file(trim(filepath),file_exists)
        open(newunit = inunit, file = trim(filepath))
        read(inunit,*, iostat = io); close(inunit)
        open(newunit = inunit, file = trim(filepath))
        if(io<0) write(inunit,*) temperature(i),'#temperature'
        close(inunit)
    end do

end subroutine create_files
