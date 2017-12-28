@echo off
rem
rem Script to run a pint self compile
rem
rem First, change elide patterns to remove prd and prr file statements.
rem The modified file goes in pintm.pas (pint modified).
rem
sed -e 's/{elide}/{/g' -e 's/{noelide}/}/g' -e 's/{remove//g' -e 's/remove}//g' source\pint.pas > pintm.pas
rem
rem Compile the final target, the PAT
rem
echo Compiling the ISO 7185 PAT
call compile standard_tests\iso7185pat
type standard_tests\iso7185pat.err
rem
rem Compile pint itself
rem
echo Compiling pint to intermediate code
call compile pintm
type pintm.err
rem
rem Add the final target program (the pat) to the end of pint.
rem This means that the version of pint will read and interpret
rem this.
rem
cat pintm.p5 standard_tests\iso7185pat.p5 > tmp.p5
del pintm.p5
ren tmp.p5 pintm.p5
rem
rem Create null input file
rem
echo.>pintm.inp
rem
rem Now run pint on pint, which runs the PAT.
rem
echo Running pint on itself, to run the ISO 7185 PAT
call run pintm
copy pintm.lst standard_tests\iso7185pats.lst > temp
del temp
echo Comparing PAT result to reference
call diffnole standard_tests\iso7185pats.lst standard_tests\iso7185pats.cmp > standard_tests\iso7185pats.dif
rem
rem Show the file, so if the length is zero, it compared ok.
rem
echo Resulting diff file length should be zero for pass
dir standard_tests\iso7185pats.dif
