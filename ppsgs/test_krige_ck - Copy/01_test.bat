@echo off
del results.dat
..\ppsgs.exe -d 2 62 4800 15320 0 -o ../test_data/pc2d.csv -g ../test_data/grid2d.csv -o2 ../test_data/aem2d.csv ^
  -v sph 5000.0 0.12 0.0 -vc sph 5000.0 0.04 0.05 -v2 sph 5000.0 0.068 0.0  ^
  --nmax 300 --nmax2 300 -vb -ns 0 -u --writexy  results.csv

..\ppsgs.exe -d 2 62 4800 15320 0 -o ../test_data/pc2d.csv ^
             -g  ../test_data/grid2d.csv ^
             -o2 ../test_data/aem2d.csv ^
             -f  fac.dat ^
             -v  sph 5000.0 0.12 0.0 -vc sph 5000.0 0.04 0.05 -v2 sph 5000.0 0.068 0.0  ^
             -r  ../test_data/path4800.csv --nmax 300 --nmax2 300 -vb -ns 0 -u

..\ppsgs.exe -d 2 62 4800 15320 0 -o ../test_data/pc2d.csv ^
             -o2 ../test_data/aem2d.csv -f fac.dat -s ../test_data/sample4800_0.csv -vb -u -fm (G0.12) results.dat



rem ..\ppsgs.exe -d 2 5 3 0 0 -o obs.csv -f fac.dat -s sample.csv -u -v lin 1.0 10.0 0.0 

rem pause