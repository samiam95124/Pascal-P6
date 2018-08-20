@echo off
rem
rem Compile pcom using IP Pascal
rem

if exist "pcom.pas" goto fileexists
echo *** Error: Missing pcom.pas file
goto stop
:fileexists

echo.
echo Compiling pcom.pas to create pcom.exe
echo.
pc pcom/standard/nrf/r

rem
rem Terminate
rem
:stop
