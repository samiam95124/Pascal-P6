@echo off
rem
rem Change version numbers on all compare files
rem
if not "%2"=="" goto paramok1
echo *** Error: Missing "from" version number
goto stop
:paramok1
if not "%3"=="" goto paramok2
echo *** Error: Missing "to" version number
goto stop
:paramok2
call chgver sample_programs\basics.cmp %1 %2
call chgver sample_programs\hello.cmp %1 %2
call chgver sample_programs\match.cmp %1 %2
call chgver sample_programs\pascals.cmp %1 %2
call chgver sample_programs\roman.cmp %1 %2
call chgver sample_programs\startrek.cmp %1 %2

call chgver standard_tests\iso7185pat.cmp
call chgver standard_tests\iso7185pats.cmp
:stop