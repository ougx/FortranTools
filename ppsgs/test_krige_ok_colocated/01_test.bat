rem second grid cell location is identical to one observation location
set "ppsgs=p:\1879 - CA-DWR Basin Characterization Project\Analysis\TextureModel\ppsgs.exe"
"%ppsgs%" -d 2 62 2 0 0 -o ..\test_data\pc2d.csv -g grid.csv -u -v sph 5000.0 0.12 0.0 -ns 0 -xy -vb -bs 0 1 grid.csv
"%ppsgs%" -d 2 62 2 0 0 -o ..\test_data\pc2d.csv -g grid.csv -u -v sph 5000.0 0.12 0.0 -ns 1 -xy -vb -bs 0 1 grid_sim.csv