call ..\..\compiler_setting_gfortran.bat

%ff%  -o test ..\rotate.f90 test.f90 

test.exe 

pause