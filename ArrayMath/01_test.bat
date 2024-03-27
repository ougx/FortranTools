@echo off
REM below reading the array from file WGMH009_RePrecip_01Jan_modelgrid.asc; 6 lines are skipped when reading the file
REM the array is multiplied by 0.1 and minus 10
REM the result is written to the file result.dat


ArrayMath -d 264 372 -s 6 -a  WGMH009_RePrecip_01Jan_modelgrid.asc - 0.1 -10 result.dat