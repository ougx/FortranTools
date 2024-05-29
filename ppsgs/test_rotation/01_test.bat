call ..\..\compiler_setting.bat
del test.exe
%ff%  -o test ..\rotate.f90 test.f90 

test.exe 

pause