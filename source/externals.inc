{*******************************************************************************

Petit-ami interface

Defines the calls that make up the Petit-ami support library for Pascaline.

Petit-ami is the collection of Pascaline support library modules written in
C.

*******************************************************************************}

{*******************************************************************************

External routine definitions

Each of these brings an externally defined routine into Pascal callable form.

*******************************************************************************}

{ Allow GNU Pascal extensions }
{$gnu-pascal}

function extendroutine(j, i: integer): integer; external name 'extend_routine';

{ Restore to ISO 7185 Pascal language }
{$classic-pascal-level-0}

{*******************************************************************************

Lookup symbol/module name

Given a module name and symbol name, returns a number used to execute the given
module call. If no routine is found, a zero is returned. If a routine is found,
this can be used as a key to execute the correct routine.

*******************************************************************************}

procedure LookupExternal(
    { name of module }           var module: symnam;
    { symbol within module }     var symbol: symnam;
    { resulting routine number } var routine: integer
    );

begin

   routine := 0; { set no routine found }

   if module = 'extensions          ' then begin

       if symbol = 'extend_routine      ' then routine := 1

   end

end;

{*******************************************************************************

Execute routine by number

Given a routine number, executes that routine. All of the input parameters are
passed on the stack, and the result, if any, also returned on the stack.

All of the input parameters are removed from the stack, leaving just the result
(if any).

The load of parameters is fairly ad-hoc. Value parameters are simply fetched.
VAR and VIEW pameters have to be loaded into a buffer to transfer.

*******************************************************************************}

procedure ExecuteExternal(
    { number of routine to execute} routine: integer
    );

var a, b, c: integer;

begin

    case routine of

        1: begin

            a := getint(sp+intsize);
            b := getint(sp);
            c := extendroutine(a, b);
            putint(mp, c)

        end

    end

end;

{*******************************************************************************

Get number of routines

Retrieves the total number of routines available.

*******************************************************************************}

function NumExternal: integer;

begin

    NumExternal := 1

end;
