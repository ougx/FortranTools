call ..\compiler_setting_gfortran.bat

rem  -g -fcheck=all
echo %ff%  -g -fcheck=all ..\common\f90getopt.F90 ..\sorting\mrgref.f90 ArrayMath.f90 -o ArrayMath
%ff%   -o ArrayMath ^
  ..\common\str_upper.f90  ^
  ..\common\f90getopt.F90  ^
  ..\sorting\mrgref.f90    ^
  ..\minv\ieeeck.f         ^
  ..\minv\iparmq.f         ^
  ..\minv\ilaenv.f         ^
  ..\minv\lsame.f          ^
  ..\minv\sgemm.f          ^
  ..\minv\sgemv.f          ^
  ..\minv\sroundup_lwork.f ^
  ..\minv\sswap.f          ^
  ..\minv\sscal.f          ^
  ..\minv\strmv.f          ^
  ..\minv\strmm.f          ^
  ..\minv\strsm.f          ^
  ..\minv\strti2.f         ^
  ..\minv\strtri.f         ^
  ..\minv\xerbla.f         ^
  ..\minv\isamax.f         ^
  ..\minv\slamch.f         ^
  ..\minv\slaswp.f         ^
  ..\minv\sgetrf.f         ^
  ..\minv\sgetrf2.f        ^
  ..\minv\sgetri.f         ^
  ArrayMath.f90 

pause