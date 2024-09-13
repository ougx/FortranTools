@echo off

..\ppsgs.exe -d 2 62 4 15320 0 -of ../test_data/pc2d.csv -gf ../test_data/grid2d.csv -o2 ../test_data/aem2d.csv ^
  -v1 sph 5000.0 0.12 0.0 -vc sph 5000.0 0.04 0.05 -v2 sph 5000.0 0.068 0.0  ^
  --nmax 200 --nmax2 200 -v -u --writexy -wm grid.csv


..\ppsgs.exe -d 2 62 4800 15320 0 ^
             -of ../test_data/pc2d.csv ^
             -gf ../test_data/grid2d.csv ^
             -o2 ../test_data/aem2d.csv ^
             -v1 sph 5000.0 0.12 0.0 ^
             -vc sph 5000.0 0.04 0.05 ^
             -v2 sph 5000.0 0.068 0.0  ^
             --nmax 300 --nmax2 300 -v -u -xy -bd 0.0 1.0 ^
             grid.csv

..\ppsgs.exe -d 2 62 4800 15320 0 ^
             -of ../test_data/pc2d.csv ^
             -gf ../test_data/grid2d.csv ^
             -o2 ../test_data/aem2d.csv ^
             -v1 sph 5000.0 0.12 0.0 ^
             -vc sph 5000.0 0.04 0.05 ^
             -v2 sph 5000.0 0.068 0.0  ^
             -pf ../test_data/path4800.csv ^
             -sf ../test_data/sample4800_0.csv ^
             --nmax 300 --nmax2 300 -v -u -xy -s  -bd 0.0 1.0 ^
             grid_sim.csv

rem ..\ppsgs.exe -d 2 62 4800 15320 0 ^
rem   -of ../test_data/pc2d.csv ^
rem   -o2 ../test_data/aem2d.csv ^
rem   -ff fac.dat ^
rem   -sf ../test_data/sample4800_0.csv ^
rem   -v -u -fm (G0.12) -sa grid_sim.dat



rem ..\ppsgs.exe -d 2 5 3 0 0 -o obs.csv -f fac.dat -s sample.csv -u -v lin 1.0 10.0 0.0 

pause