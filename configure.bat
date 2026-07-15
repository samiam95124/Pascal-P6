@echo off
rem ################################################################################
rem #
rem # Configure script for Pascal-p6
rem #
rem # Sets up the complete Pascal-p6 project: determines the host, architecture
rem # and bit length this machine presents, and restores the working bin and libs
rem # directories from the matching directory of the hosts tree. The hostinstall
rem # make target performs the reverse snapshot.
rem #
rem # This is the Windows batch counterpart of the "configure" bash script; the
rem # two mirror each other.
rem #
rem ################################################################################

rem
rem Determine the running host, architecture and bit length, matching the
rem makefile's detection. This script runs on Windows, so the host is windows;
rem the architecture and bit length come from the processor environment
rem (PROCESSOR_ARCHITEW6432 is defined for a 32 bit process on a 64 bit system).
rem
set host=windows
set arch=x86
set bits=64
if /i "%PROCESSOR_ARCHITECTURE%" == "AMD64" (

    set arch=x86
    set bits=64

) else if /i "%PROCESSOR_ARCHITECTURE%" == "ARM64" (

    set arch=arm
    set bits=64

) else if /i "%PROCESSOR_ARCHITECTURE%" == "x86" (

    if defined PROCESSOR_ARCHITEW6432 (

        set arch=x86
        set bits=64

    ) else (

        set arch=x86
        set bits=32

    )

)

rem
rem Determine if needed programs exist. The only fatal one is grep, because we
rem need that to run this script. The rest will impact the running of various
rem test and build scripts.
rem
where /q grep
if errorlevel 1 (

    echo *** No grep was found
    echo Terminating
    exit /b 1

)

call :checkexists diff
call :checkexists sed
call :checkexists rm
call :checkexists cp
call :checkexists mv

rem
rem flip (the Unix/DOS end of line changer) is built from source if it is not
rem already present.
rem
where /q flip
if errorlevel 1 (

    echo flip does not exist, attempting to make it
    gcc -o bin\flip.exe source\flip.c
    if not exist bin\flip.exe echo *** Unable to make flip

)

call :checkexists ls
call :checkexists zip

rem
rem Check user arguments
rem
for %%x in (%*) do (

    if "%%x" == "--help" (

        echo Configure host for Pascal-p6
        echo.
        echo --32:        Select 32 bit target
        echo --64:        Select 64 bit target
        echo.
        exit /b 0

    ) else if "%%x" == "--32" (

        set bits=32

    ) else if "%%x" == "--64" (

        set bits=64

    )

)

rem
rem Restore the working binaries and libraries from the hosts tree
rem
set hostcell=hosts\%host%\%arch%\bit%bits%
echo Set up for %host%/%arch%/bit%bits%

if not exist "%hostcell%\bin" (

    echo *** No hosts tree entry for %hostcell%
    exit /b 1

)

copy /y "%hostcell%\bin\*" bin >nul 2>&1
copy /y "%hostcell%\libs\*" libs >nul 2>&1
if exist bin\.gitkeep del /q bin\.gitkeep
if exist libs\.gitkeep del /q libs\.gitkeep

echo Configure completed!
exit /b 0

rem
rem Check command exists; warn if not (non fatal).
rem
:checkexists
where /q %1
if errorlevel 1 echo *** No %1 was found
goto :eof
