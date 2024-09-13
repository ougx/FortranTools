call ..\compiler_setting_gfortran.bat

del ppsgs.exe *.obj *.o *.mod *.pdb
rem -real-size:64
rem -fdefault-real-8
%ff% -fdefault-real-8 -o ppsgs kdtree2_mask.f90 sspsv.f rotate.f90 normal.f90 variogram.f90 gaussian_quadrature.f90 ^
  ..\common\f90getopt.F90  ^
  ..\common\progress_bar.F90  ^
  ppsgs.F90

rem  ..\sorting\mrgref.f90  ^