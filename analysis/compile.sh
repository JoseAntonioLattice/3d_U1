
gfortran -c /home/estudiante/Documents/Fortran/modules/src/precision_mod.f95
gfortran -c /home/estudiante/Documents/Fortran/modules/src/statistics.f95
gfortran -c /home/estudiante/Documents/Fortran/modules/src/number2string_mod.f95
gfortran -c /home/estudiante/Documents/Fortran/modules/src/get_array.f95
gfortran -c src/analysis_outeq.f95


gfortran *.o


./a.out
