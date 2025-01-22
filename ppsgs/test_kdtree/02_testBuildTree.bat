call ..\..\compiler_setting_gfortran.bat

%ff% -o test ..\kdtree2_mask.f90 kdtree2_testBuildTree.f90

test.exe 

pause