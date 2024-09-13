call ..\..\compiler_setting_gfortran.bat

%ff%  -fdefault-real-8 -o test ..\rotate.f90 test.f90 

test.exe 

pause