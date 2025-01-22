@echo off


gfortran -cpp -fbacktrace -ffree-line-length-none -O2 -static -s  -fdefault-real-8 -o test3 ..\variogram.f90 test3.f90 
test3.exe


pause