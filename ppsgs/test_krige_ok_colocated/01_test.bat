rem second grid cell location is identical to one observation location
set "ppsgs=..\ppsgs.exe"
"%ppsgs%" -d 2 62 2 0 0 -of ..\test_data\pc2d.csv -gf ..\test_data\grid2pnt.csv -u -v1 sph 5000.0 0.12 0.0    -xy -v -bd 0 1 grid.csv
"%ppsgs%" -d 2 62 2 0 0 -of ..\test_data\pc2d.csv -gf ..\test_data\grid2pnt.csv -u -v1 sph 5000.0 0.12 0.0 -s -xy -v -bd 0 1 grid_sim.csv