{*******************************************************************************

Null externals interface

This module just stubs off the externals interface in pint.pas. It is used to
satisfy references when the externals package is not used.

It can also be used as a template for an externals package.

*******************************************************************************}

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
    { name of module }           var modulen: symnam;
    { symbol within module }     var symbol:  symnam;
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
    { address of parameters bottom/result } var params:  address
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
