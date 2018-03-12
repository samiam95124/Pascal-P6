@echo off
rem
rem Run a Pascal file in batch mode using GPC Pascal
rem
rem Runs a Pascal intermediate in batch mode.
rem
rem Execution:
rem
rem run <file> [mach|pint] [inp]
rem
rem <file> is the filename without extention.
rem
rem The files are:
rem
rem <file>.p6  - The intermediate file
rem <file>.out - The prr file produced
rem <file>.inp - The input file to the program
rem <file>.lst - The output file from the program
rem <file>.p6o - The object deck for mach, if selected
rem
rem The "mach" parameter changes the run to include the mach interpreter. To do
rem this the mach compile must be used -- we can't insert the object option to 
rem intermediate here. If mach is selected, the .p6o file is also created.
rem
rem The "inp" parameter gives a file to be concatenated with the run input file.
rem This will be .p6 or .p6o depending on the run mode. When programs read their
rem input from the prd file, this is required. Otherwise it is left blank. To 
rem satisfy the mach parameter in this case, use "pint".
rem

if "%1"=="" (

    echo *** Error: Missing parameter
    exit /b 1

)

if not exist "%1.p6" (

    echo *** Error: Missing %1.p6 file
    exit /b 1

)

if not exist "%1.inp" (

    echo *** Error: Missing %1.inp file
    exit /b 1

)

if "%2"=="mach" (

    cp %1.p6 prd
    pint
    cat prr %3 > %1.p6o
    cp %1.p6o prd
    pmach < %1.inp > %1.lst 2>&1
    
) else (

    cat %1.p6 %3 > prd
    pint < %1.inp > %1.lst 2>&1
    
)
if exist "%1" rm %1.out
mv prr %1.out
chmod +w %1.out
