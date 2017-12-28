@echo off
rem
rem Script to test p2 compile and run
rem
rem Compile p2
rem
echo Compling pcomp to intermediate code
call compile p2\pcomp
type p2\pcomp.err
rem
rem Copy the test file to the input file and compile it via interpreted p2
rem
cp p2\roman.pas p2\pcomp.inp
call run p2\pcomp
cat p2\pcomp.lst
rem
rem For neatness sake, copy out the intermediate to .p2 file
rem
cp p2\pcomp.out p2\roman.p2
rem
rem Compile pasint
rem
echo Compiling pasint to intermediate code
call compile p2\pasint
rem
rem Add the final target program to the end of pasint.
rem This means that the version of pint will read and interpret
rem this.
rem
rem For those of you having fun reading this, yes, the next statement accurately
rem describes what is going on: we are concatenating and running two different
rem intermediate codes together in completely different formats!
rem
cat p2\pasint.p5 p2\roman.p2 > tmp.p5
rm p2\pasint.p5
mv tmp.p5 p2\pasint.p5
rem
rem Create null input file
rem
echo.>p2\pasint.inp
rem
rem Now run pasint on pint, which runs the test program.
rem
echo Running pasint on pint to execute test program
call run p2\pasint
rem
rem Copy the result listing back to roman.lst, again for neatness
rem
cp p2\pasint.lst p2\roman.lst
rem
rem Now compare with reference
rem
echo Comparing PAT result to reference
call diffnole p2\roman.lst p2\roman.cmp > p2\roman.dif
rem
rem Show the file, so if the length is zero, it compared ok.
rem
echo Resulting diff file length should be zero for pass
dir p2\roman.dif
del p2\pcomp.inp
del p2\pasint.inp