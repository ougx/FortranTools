@echo off

..\ppsgs.exe -d 2 4 1 0 0 -of obs1.csv -gf grid1.csv -u -v1 sph 1.0 1.0 0.0
..\ppsgs.exe -d 2 4 1 0 0 -of obs2.csv -gf grid2.csv -u -v1 sph 1.0 1.0 0.0
..\ppsgs.exe -d 2 4 1 0 0 -of obs3.csv -gf grid3.csv -u -v1 sph 1.0 1.0 0.0
..\ppsgs.exe -d 2 4 1 0 0 -of obs4.csv -gf grid4.csv -u -v1 sph 1.0 1.0 0.0
..\ppsgs.exe -d 2 4 1 0 0 -of obs5.csv -gf grid5.csv -u -v1 sph 1.0 1.0 0.0

rem 1.12750000000 1.10500000000 1.06000000000 0.877500000000 0.810000000000
echo.
echo validation
..\ppsgs.exe -d 2 5 0 0 0 -of obs.csv -cv -u -v1 sph 1.0 1.0 0.0
pause