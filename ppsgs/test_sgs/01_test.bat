@echo off

..\ppsgs.exe -d 2 5 3 0 0 ^
  -of  ..\test_data\obs_simple.csv ^
  -gf  ..\test_data\grid_simple.csv ^
  -pf ..\test_data\path3.csv ^
  -ff fac.dat -u -v1 lin 1.0 10.0 0.0 -wm -v -s

..\ppsgs.exe -d 2 5 3 0 0 -s -of ..\test_data\obs_simple.csv -ff fac.dat -sf ..\test_data\sample4800_0.csv -u -v1 lin 1.0 10.0 0.0  -v


echo correct answer:
echo 0.996000000000 1.23300000000 1.29020880698

pause