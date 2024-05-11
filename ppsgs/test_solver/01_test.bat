call ..\..\compiler_setting_gfortran.bat

%ff% -freal-4-real-8 -o test ..\sspsv.f test.f90 

test.exe 

pause