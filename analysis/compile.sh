
gfortran -c /home/jose/Documents/Fortran/modules/precision_mod.f95
gfortran -c /home/jose/Documents/Fortran/modules/statistics.f95
gfortran -c /home/jose/Documents/Fortran/modules/number2string_mod.f95
gfortran -c /home/jose/Documents/Fortran/modules/get_array.f95
gfortran -c analysis_outeq.f95


gfortran *.o


./a.out
