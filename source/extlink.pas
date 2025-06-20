{*******************************************************************************

Pascaline externals interface

This module just stubs off the externals interface in pint.pas. The Pascaline
interface is not defined at this time.

*******************************************************************************}

module extlink;

{*******************************************************************************

External routine definitions

Each of these brings an externally defined routine into Pascal callable form.

*******************************************************************************}

{ Place external routine declarations here }

{*******************************************************************************

Lookup symbol/module name

Given a module name and symbol name, returns a number used to execute the given
module call. If no routine is found, a zero is returned. If a routine is found,
this can be used as a key to execute the correct routine.

*******************************************************************************}

procedure LookupExternal(
    { name of module }           view modulen: string;
    { symbol within module }     view symbol:  string;
    { resulting routine number } var routine: integer
    );

begin

   routine := 0; { set no routine found }

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
    { number of routine to execute}             routine: integer;
    { address of parameters bottom/result } var params:  integer
    );

begin

    { place external routine executions here }

end;

{*******************************************************************************

Get number of routines

Retrieves the total number of routines available.

*******************************************************************************}

function NumExternal: integer;

begin

    NumExternal := 0

end;

begin
end.