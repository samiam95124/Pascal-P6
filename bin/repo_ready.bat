@echo off
rem
rem Make p5 ready for GIT
rem
rem This script fixes up the GIT directories to the form that GIT
rem expects. This minimizes the differences between the P5 directories
rem and the GIT repository in preparation for checkins.
rem
call configure --gpc --64
make clean