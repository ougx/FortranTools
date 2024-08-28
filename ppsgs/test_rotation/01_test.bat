call ..\..\compiler_setting_gfortran.bat
del test.exe
%ff%  -o test ..\rotate.f90 test.f90 

test.exe 

pause