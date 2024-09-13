@echo off

..\ppsgs.exe -d 2 5 1 0 0 -of obs.csv -gf grid01.csv -ff fac01.dat -u -v1 lin 10.0 1.0 0.0
..\ppsgs.exe -d 2 5 1 0 0 -of obs.csv -ff fac01.dat -sf sample.csv -u -v1 lin 10.0 1.0 0.0
echo Correct result: 0.7796277

..\ppsgs.exe -d 2 5 1 0 0 -of obs.csv -gf grid02.csv -ff fac02.dat -u -v1 lin 10.0 1.0 0.0 
..\ppsgs.exe -d 2 5 1 0 0 -of obs.csv -ff fac02.dat -sf sample.csv -u -v1 lin 10.0 1.0 0.0 
echo Correct result: 1.1827978

..\ppsgs.exe -d 2 5 1 0 0 -of obs.csv -gf grid03.csv -ff fac03.dat -u -v1 lin 10.0 1.0 0.0
..\ppsgs.exe -d 2 5 1 0 0 -of obs.csv -ff fac03.dat -sf sample.csv -u -v1 lin 10.0 1.0 0.0
echo Correct result: 1.5879789

pause