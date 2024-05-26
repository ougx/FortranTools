@echo off
REM below reading the array from file WGMH009_RePrecip_01Jan_modelgrid.asc; 6 lines are skipped when reading the file
REM the array is multiplied by 0.1 and minus 10
REM the result is written to the file result.dat

REM test reading skipping row
ArrayMath -d 5 6 -sr 1 -a  Mass.dat - 0.1 


REM test matrix inversion
ArrayMath -d 10 10  -a forMinv.dat - 1.0 -x matinv > forMinv_inv.dat
ArrayMath -d 10 10  -a forMinv.dat - 1.0 --dot forMinv_inv.dat


REM test filtering
ArrayMath -d 2063 -cn -a  Mass.dat - 1.0 -f row==44 -s c2 -x diff > mass_dt.out
