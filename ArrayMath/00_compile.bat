call ..\compiler_setting.bat

echo %ff%  ..\common\f90getopt.F90 ..\sorting\mrgref.f90 ArrayMath.f90 -o ArrayMath
%ff%  ..\common\str_upper.f90  ..\common\f90getopt.F90 ..\sorting\mrgref.f90 ArrayMath.f90 -o ArrayMath