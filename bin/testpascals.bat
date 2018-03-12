@echo off
rem
rem Script to test pascals
rem
rem Compile pascals
rem
call compile sample_programs\pascals %1
rem
rem Run that with the input file appended to the intermediate/object
rem
if "%1"=="" (

    call run sample_programs\pascals int sample_programs\pascals.inp
    
) else (

    call run sample_programs\pascals %1 sample_programs\pascals.inp
    
)
rem
rem Compare to reference
call diffnole sample_programs\pascals.lst sample_programs\pascals.cmp > sample_programs\pascals.dif
rem
rem Show the file, so if the length is zero, it compared ok.
rem
echo Resulting diff file length should be zero for pass
dir sample_programs\pascals.dif