{*******************************************************************************
*                                                                              *
*                       Terminal external execution                            *
*                                                                              *
* The graphics flavor of the external execution module, selected for the      *
* pintg interpreter by module path. Joining graphics links the "blonde"       *
* graphics archive (also placed in this directory): the model installs no     *
* automatic window over the interpreter's standard files, so the interpreter  *
* keeps its own console. The vmhost hook then opens a window through openwin  *
* and gives it to the interpreted program: the window's files are placed in   *
* the general file table and the interpreted standard input and output file   *
* cells point at them, so all of the program's standard I/O flows to the      *
* window through the existing general file machinery. The interpreter's own   *
* messages and the interpreted error file stay on the console.                *
*                                                                              *
* Terminal externals error as not implemented under this flavor (a graphics   *
* program does not use the terminal module).                                  *
*                                                                              *
*******************************************************************************}

module extterm;

joins graphics; { graphics model I/O (the blonde archive) }

uses pint_mem; { low level vm access for pint }

procedure execterminal(routine: integer; var params: integer); forward;

private

const FunctionNotImplemented = 90;

var nilwin: text; { never-opened file: openwin takes it as no parent }

{ find a free general file slot }

function frefil: fileno;

var i, ff: integer;

begin

   ff := 0;
   for i := commandfn+1 to maxfil do
      if (filstate[i] = fnone) and (ff = 0) then ff := i;
   if ff = 0 then errore(FunctionNotImplemented);
   frefil := ff

end;

{ host the interpreted program in its own window }

override procedure vmhost;

var fi, fo: fileno;

begin

   fi := frefil; filstate[fi] := fread; { reserve the input side }
   fo := frefil; filstate[fo] := fwrite; { reserve the output side }
   { open the window against the file table entries }
   graphics.openwin(filtable[fi], filtable[fo], nilwin, 1);
   { the interpreted standard files attach to the window }
   vmstdin := fi;
   vmstdout := fo

end;

procedure execterminal(routine: integer; var params: integer);

begin

   refer(routine);
   refer(params);

   errore(FunctionNotImplemented)

end;

begin
end.
