@echo off
rm fac.dat*
set start=%time%
..\ppsgs.exe -d 3 1206 4800 66339 0 ^
	-of "..\test_data\obs3d_clay.csv" ^
	-o2 "..\test_data\aem3d_clay.csv" ^
	-gf "..\test_data\gridxyz.csv" ^
	-v1 sph 5000.0 0.12 0.0 ^
	-vc sph 5000.0 0.04 0.05 ^
	-v2 sph 5000.0 0.068 0.0  ^
	-u --anis2 0.5 -bd 0.0 1.0^
	--nmax 100 --nmax2 100^
	-v -xy grid.csv

set end=%time%
set options="tokens=1-4 delims=:.,"
for /f %options% %%a in ("%start%") do set start_h=%%a&set /a start_m=100%%b %% 100&set /a start_s=100%%c %% 100&set /a start_ms=100%%d %% 100
for /f %options% %%a in ("%end%") do set end_h=%%a&set /a end_m=100%%b %% 100&set /a end_s=100%%c %% 100&set /a end_ms=100%%d %% 100

set /a hours=%end_h%-%start_h%
set /a mins=%end_m%-%start_m%
set /a secs=%end_s%-%start_s%
set /a ms=%end_ms%-%start_ms%
if %ms% lss 0 set /a secs = %secs% - 1 & set /a ms = 100%ms%
if %secs% lss 0 set /a mins = %mins% - 1 & set /a secs = 60%secs%
if %mins% lss 0 set /a hours = %hours% - 1 & set /a mins = 60%mins%
if %hours% lss 0 set /a hours = 24%hours%
if 1%ms% lss 100 set ms=0%ms%

:: Mission accomplished
set /a totalsecs = %hours%*3600 + %mins%*60 + %secs%
echo command took %hours%:%mins%:%secs%.%ms% (%totalsecs%.%ms%s total)

pause