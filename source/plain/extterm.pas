{*******************************************************************************
*                                                                              *
*                       Terminal external execution                            *
*                                                                              *
* The plain flavor of the external execution module: the plain interpreter    *
* hosts no terminal model, so executing a terminal external is an error. The  *
* pintt interpreter selects the real flavor in source/term by module path.    *
*                                                                              *
*******************************************************************************}

module extterm;

uses pint_mem; { low level vm access for pint }

procedure execterminal(routine: integer; var params: integer); forward;

private

const FunctionNotImplemented = 90;

procedure execterminal(routine: integer; var params: integer);

begin

   errore(FunctionNotImplemented)

end;

begin
end.
