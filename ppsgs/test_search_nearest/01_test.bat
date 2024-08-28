call ..\..\compiler_setting_gfortran.bat

del test.exe
%ff%  -fdefault-real-8 -o test ..\rotate.f90 ..\kdtree2_mask.f90 test.f90 

test.exe 

pause