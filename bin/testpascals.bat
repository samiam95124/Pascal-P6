@echo off
rem
rem Script to test pascals
rem
rem Compile pascals
rem
call compile sample_programs\pascals
rem
rem Prepare a prd deck that has the pascals intermediate first, followed by the
rem program to run.
rem
cat sample_programs\pascals.p5 sample_programs\roman.pas > sample_programs\tmp.p5
rm sample_programs\pascals.p5
cp sample_programs\tmp.p5 sample_programs\pascals.p5
rm sample_programs\tmp.p5
rem
rem Run that
rem
call run sample_programs\pascals
rem
rem Compare to reference
call diffnole sample_programs\pascals.lst sample_programs\pascals.cmp > sample_programs\pascals.dif
rem
rem Show the file, so if the length is zero, it compared ok.
rem
echo Resulting diff file length should be zero for pass
dir sample_programs\pascals.dif