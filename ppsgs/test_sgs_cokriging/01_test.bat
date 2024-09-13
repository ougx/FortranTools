@echo off

..\ppsgs.exe -d 2 62 4800 15320 0 -s ^
 -of ..\test_data\pc2d.csv ^
 -gf ..\test_data\grid2d.csv ^
 -o2 ..\test_data\aem2d.csv ^
 -v1 sph 5000.0 0.12 0.0 ^
 -vc sph 5000.0 0.04 0.05 ^
 -v2 sph 5000.0 0.068 0.0  ^
 -pf ..\test_data\path4800.csv ^
 -sf ..\test_data\sample4800.csv ^
 --nmax 300 --nmax2 300 -v -s -xy -bd 0.0 1.0 ^
 grid.csv

rem ..\ppsgs.exe -d 2 62 4800 15320 0 -s ^
rem  -of ..\test_data\pc2d.csv ^
rem  -gf ..\test_data\grid2d.csv ^
rem  -o2 ..\test_data\aem2d.csv ^
rem  -ff fac.dat ^
rem  -v1 sph 5000.0 0.12 0.0 ^
rem  -vc sph 5000.0 0.04 0.05 ^
rem  -v2 sph 5000.0 0.068 0.0  ^
rem  -pf path.csv ^
rem  --nmax 300 --nmax2 300 -v
rem  
rem  -d 2 62 4800 15320 0 -s -of obs.csv              -o2 rho2.csv -ff fac.dat -sf sample.csv -bs 0.0 1.0 -v -fm (G0.12) sgs.dat

pause