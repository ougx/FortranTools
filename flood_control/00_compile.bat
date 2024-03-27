call ..\compiler_setting.bat

%ff% flood_control.f90 -o flood_control
%ff% flood_control.f90 -real-size:64 -o flood_control_dbl