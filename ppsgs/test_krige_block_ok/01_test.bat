@echo off

echo OK 
..\ppsgs.exe -d 2 62 16 0 0 ^
  -of ..\test_data\pc2d.csv ^
  -gf ..\test_data\gridblockpnt2d.csv ^
  -u -v1 sph 5000.0 0.12 0.0 -xy -v ^
  -bd 0.0 1.0 grid_ok_pnts.csv
  
echo OK 
..\ppsgs.exe -d 2 62 1 0 0 ^
  -of ..\test_data\pc2d.csv ^
  -gf ..\test_data\gridblock2d.csv ^
  -u -v1 sph 5000.0 0.12 0.0 -xy -v ^
  -bd 0.0 1.0 grid_ok.csv

echo OK block kriging with customized points and weights
..\ppsgs.exe -d 2 62 16 0 0 ^
  -of ..\test_data\pc2d.csv ^
  -gf ..\test_data\gridblockpnt2d.csv ^
  -bf ..\test_data\gridblock2d.csv -bw ^
  -u -v1 sph 5000.0 0.12 0.0 -xy -v ^
  -bd 0.0 1.0 -wm grid_ok_block_customized.csv

echo OK block kriging with customized points and weights (from inverse distance)
..\ppsgs.exe -d 2 62 16 0 0 ^
  -of ..\test_data\pc2d.csv ^
  -gf ..\test_data\gridblockpnt2d_uniform.csv ^
  -bf ..\test_data\gridblock2d.csv -bw ^
  -u -v1 sph 5000.0 0.12 0.0 -xy -v ^
  -bd 0.0 1.0 -wm grid_ok_block_customized_inv.csv > grid_ok_block_customized_inv.log
  
echo OK block kriging with uniform block size
..\ppsgs.exe -d 2 62 1 0 0 ^
  -of ..\test_data\pc2d.csv ^
  -gf ..\test_data\gridblock2d.csv ^
  -bs 2000 2000 ^
  -u -v1 sph 5000.0 0.12 0.0 -xy -v ^
  -bd 0.0 1.0 -wm grid_ok_block_uniform.csv > grid_ok_block_uniform.log
pause