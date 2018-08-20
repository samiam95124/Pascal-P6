@echo off
rem
rem Change version numbers on compare file
rem
rem Format:
rem
rem chgver file from_ver to_ver
rem
if not "%1"=="" goto paramok1
echo *** Error: Missing file name
goto stop
:paramok1
if not "%2"=="" goto paramok2
echo *** Error: Missing "from" version number
goto stop
:paramok2
if not "%3"=="" goto paramok3
echo *** Error: Missing "to" version number
goto stop
:paramok3
if exist "%1.pas" goto fileexists
echo *** Error: Missing "%1" file
goto stop
:fileexists
sed -e 's/P5 Pascal interpreter vs. %2/P5 Pascal interpreter vs. %3/g' %1 > temp
copy temp %1
del temp
:stop
