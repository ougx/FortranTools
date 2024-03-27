@echo off
set ff=gfortran -cpp -fbacktrace -ffree-line-length-none -O2 -static -s
set cc=cl /O2 /fp:except /nologo /c
set lk=link -static 
