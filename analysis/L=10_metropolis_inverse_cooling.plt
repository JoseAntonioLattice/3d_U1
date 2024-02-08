
set logscale


set title "Metropolis, L = 16, T_f = 0" font ",20"
set xlabel "t + {/Symbol t}_Q + 1" font",14"
set ylabel "monopole density" font ",14"


pl [1:5000][0.02:0.2]"L=16_tau_Q=8_metropolis.dat" u ($0+1):4:5 w e t "{/Symbol t}_Q = 8",\
                     "L=16_tau_Q=16_metropolis.dat" u ($0+1):4:5 w e t "{/Symbol t}_Q = 16",\
                     "L=16_tau_Q=32_metropolis.dat" u ($0+1):4:5 w e t "{/Symbol t}_Q = 32",\
                     "L=16_tau_Q=64_metropolis.dat" u ($0+1):4:5 w e t "{/Symbol t}_Q = 64", \
                     "L=16_tau_Q=128_metropolis.dat" u ($0+1):4:5 w e t "{/Symbol t}_Q = 128",\
                     "L=16_tau_Q=256_metropolis.dat" u ($0+1):4:5 w e t "{/Symbol t}_Q = 256", \
                     "L=16_tau_Q=512_metropolis.dat" u ($0+1):4:5 w e t "{/Symbol t}_Q = 512",\
                     "L=16_tau_Q=1024_metropolis.dat" u ($0+1):4:5 w e t "{/Symbol t}_Q = 1024"
