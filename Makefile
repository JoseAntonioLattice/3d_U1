FC = gfortran
FFLAGS = -O3 -Wall -Wextra -pedantic -std=f2008
SRC = /home/jose/Documents/Fortran/modules/precision_mod.f95 parameters_mod.f95 arrays_mod.f95 read_input.f95 memory_mod.f95 main.f95
OBJ = ${SRC:.f95=.o}


%.o: %.f95
	$(FC) $(FFLAGS) -o $@ -c $<

3dU1_out_eq.exe: $(OBJ)
	$(FC) -o $@ $(OBJ)
