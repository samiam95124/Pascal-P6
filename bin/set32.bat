@echo off
rem
rem Convert pcom and pint to 32 bit definitions
rem

call :replaceinfile source\pcom.pas
call :replaceinfile source\pint.pas
call :replaceinfile source\pmach.pas
sed "s/^#define WRDSIZ64 1/\/\* #define WRDSIZ64 1 \*\//" source\cmach.c > temp
sed "s/^\/\* #define WRDSIZ32 1 \*\//#define WRDSIZ32 1/" < temp > source\cmach.c

exit /b

: replaceinfile

rem echo Will execute: replaceinfile %1
    call :replace intsize %1
    call :replace intdig %1
    call :replace inthex %1
    call :replace ptrsize %1
    call :replace adrsize %1
    call :replace stackal %1
    call :replace stackelsize %1
    call :replace marksize %1
    call :replace begincode %1
    call :replace markfv %1
    call :replace marksl %1
    call :replace markdl %1
    call :replace markep %1
    call :replace marksb %1
    call :replace market %1
    call :replace markra %1
    call :replace ujplen %1

exit /b

:replace

rem echo Will execute: replace %1 %2
    sed "s/%1\(\s*\)=\(\s*\){\([0-9,-][0-9,-]*\)}\(\s*\)\([0-9,-][0-9,-]*\)/%1\1=\2\3\4{\5}/" < %2 > temp
    cp temp %2

exit /b
