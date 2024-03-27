@echo off
set ff=ifort -traceback -heap-arrays -O2 -fpe:0 -fpp -nowarn -nologo -static
set cc=cl /O2 /fp:except /nologo /c
set lk=link -static 

REM other switch 
REM  -real-size:64
