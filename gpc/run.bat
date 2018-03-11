@echo off
rem
rem Run a Pascal file in batch mode using GPC Pascal
rem
rem Runs a Pascal intermediate in batch mode.
rem
rem Execution:
rem
rem run <file> [mach]
rem
rem <file> is the filename without extention.
rem
rem The files are:
rem
rem <file>.pint - The intermediate file
rem <file>.out  - The prr file produced
rem <file>.inp  - The input file to the program
rem <file>.lst  - The output file from the program
rem <file>.mach - The object deck for mach, if selected
rem
rem The "mach" parameter changes the run to include the mach interpreter. To do
rem this the mach compile must be used -- we can't insert the object option to 
rem pint here. If mach is selected, the .mach file is also created.
rem

if "%1"=="" (

    echo *** Error: Missing parameter
    exit /b 1

)

if not exist "%1.pint" (

    echo *** Error: Missing %1.pint file
    exit /b 1

)

if not exist "%1.inp" (

    echo *** Error: Missing %1.inp file
    exit /b 1

)

if "%2"=="mach" (

    cp %1.pint prd
    pint
    cp prr %1.mach
    cp prr prd
    pmach < %1.inp > %1.lst 2>&1
    
) else (

    cp %1.pint prd
    pint < %1.inp > %1.lst 2>&1
    
)
if exist "%1" rm %1.out
mv prr %1.out
chmod +w %1.out
