

gfortran -O3 -Wall -Wextra -pedantic -c /home/jose/Documents/Fortran/modules/precision_mod.f95

gfortran -O3 -Wall -Wextra -pedantic -c /home/jose/Documents/Fortran/modules/number2string_mod.f95

gfortran -O3 -Wall -Wextra -pedantic -c /home/jose/Documents/Fortran/modules/check_files_directories_mod.f95

gfortran -O3 -Wall -Wextra -pedantic -c /home/jose/Documents/Fortran/modules/maths_mod.f95

gfortran -O3 -Wall -Wextra -pedantic -c /home/jose/Documents/Fortran/modules/periodic_boundary_conditions_mod.f95

gfortran -O3 -Wall -Wextra -pedantic -c /home/jose/Documents/Fortran/modules/local_update_algorithms.f95

gfortran -O3 -Wall -Wextra -pedantic -c plaquette.f95

gfortran -O3 -Wall -Wextra -pedantic -c observables_mod.f95
gfortran -O3 -Wall -Wextra -pedantic -c parameters_mod.f95
gfortran -O3 -Wall -Wextra -pedantic -c arrays_mod.f95
gfortran -O3 -Wall -Wextra -pedantic -c read_input.f95
gfortran -O3 -Wall -Wextra -pedantic -c memory_mod.f95
gfortran -O3 -Wall -Wextra -pedantic -c cooling_mod.f95
gfortran -O3 -Wall -Wextra -pedantic -c create_files.f95
gfortran -O3 -Wall -Wextra -pedantic -c dynamics_mod.f95
gfortran -O3 -Wall -Wextra -pedantic -c main.f95

gfortran *.o -o main.exe
