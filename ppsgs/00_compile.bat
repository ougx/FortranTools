call ..\compiler_setting.bat
del ppsgs.exe *.obj *.o *.mod 
rem -freal-4-real-8
%ff% -o ppsgs kdtree2_mask.f90 sspsv.f rotate.f90 normal.f90 variogram.f90 ^
  ..\common\f90getopt.F90  ^
  ..\common\progress_bar.f90  ^
  ..\sorting\inssor.f90  ^
  ppsgs.f90

rem  ..\sorting\mrgref.f90  ^