..\ppsgs.exe -d 2 62 4800 0 0 -of ..\test_data\pc2d.csv -gf ..\test_data\grid2d.csv -u -v1 sph 5000.0 0.12 0.0    -xy -v -bd 0 1 grid.csv

..\ppsgs.exe -d 2 62 4800 0 0 -of ..\test_data\pc2d.csv -gf ..\test_data\grid2d.csv -u -v1 sph 5000.0 0.12 0.0  -a1 45 -s1 0.5 -xy -v -bd 0 1 grid_rotated.csv


..\ppsgs.exe -d 2 62 4800 0 0 ^
  -of ..\test_data\pc2d.csv ^
  -gf ..\test_data\grid2d.csv ^
  -sf ..\test_data\sample4800.csv ^
  -pf ..\test_data\path4800.csv ^
  -u -v1 sph 5000.0 0.12 0.0 -s -xy -v -bd 0 1 -sa grid_sim.csv

pause