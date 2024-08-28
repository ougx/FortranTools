call ..\..\compiler_setting_gfortran.bat

%ff% -o test ..\kdtree2.f90 kdtree2_test.f90

test.exe 

pause