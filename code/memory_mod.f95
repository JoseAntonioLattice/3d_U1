module memory_mod

    use parameters_mod
    use arrays_mod

    contains

    subroutine set_memory()

        allocate(phi(L,L,L,3))
        allocate(temperature(-tau_Q:tau_Q))

        !allocate(temperature(10))
        !temperature = [0.2_dp, 0.3_dp, 0.4_dp, 0.5_dp, 0.6_dp,0.7_dp,0.8_dp,0.9_dp,1.1_dp,1.2_dp]
    end subroutine  set_memory

end module memory_mod
