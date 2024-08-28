@echo off

..\ppsgs.exe -d 2 5 3 0 0 -o ..\test_data\obs_simple.csv -g ..\test_data\grid_simple.csv -f fac.dat -u -v lin 1.0 10.0 0.0 -wm
..\ppsgs.exe -d 2 5 3 0 0 -o ..\test_data\obs_simple.csv -f fac.dat -s ..\test_data\sample4800_0.csv -u -v lin 1.0 10.0 0.0 


echo correct answer:
echo 0.779627621174 1.34293043613 1.58797907829

pause