@echo off
rem
rem Compile with P5 using GPC
rem
rem Execute with:
rem
rem p6 <sourcefile> [<sourcefile>]...
rem
rem where <sourcefile> is the name of the source file without
rem extention. The Pascal input file(s) are compiled and run
rem as a group. Any compiler errors are output to the screen.
rem Input and output to and from the running program are from
rem and to the console.
rem
rem The intermediate code is placed in <file>.p6.
rem

rem
rem Check any input files exist
rem
if "%1"=="" (

    echo *** Must specify at least one file
    goto stop
    
)

rem
rem Compile all input files into intermediate form
rem
rm -f temp.p6
for %%x in (%*) do (

    if not exist "%%~x.pas" (
  
        echo %%~x.pas does not exist
        goto stop
      
    )
    echo Compiling %%~x...
    cp %%~x.pas prd
    pcom
    mv prr %%~x.p6
    cat %%~x.p6 >> temp.p6 

)

rem
rem Run combined file
rem
mv temp.p6 prd
pint
:stop
rem rm -f prd
rem rm -f prr