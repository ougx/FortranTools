..\ppsgs.exe -d 2 62 4800 0 0 -o obs.csv -g grid.csv -f fac.dat -v sph 5000.0 0.12 0.0 -bs 0.0 1.0 -c -vb
..\ppsgs.exe -d 2 62 4800 0 0 -o obs.csv -f fac.dat  -s fac.dat_sample -vb -bs 0.0 1.0 -fm (G0.12) sgs.dat

..\ppsgs.exe -d 2 62 4800 0 0 -o obs.csv -g grid.csv -f fac1.dat -v sph 5000.0 0.12 0.0 -bs 0.0 1.0  -vb
..\ppsgs.exe -d 2 62 4800 0 0 -o obs.csv -f fac1.dat -s fac1.dat_sample -vb -bs 0.0 1.0 -fm (G0.12) sgs1.dat
pause