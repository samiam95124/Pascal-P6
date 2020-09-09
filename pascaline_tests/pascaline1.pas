{*******************************************************************************
*                                                                              *
*                        TEST USED MODULE FOR PASCALINE                        *
*                                                                              *
*            Copyright (C) 2008 S. A. Moore - All rights reserved              *
*                                                                              *
* Gives a sample module to be "used" in the Pascaline main test.               *
*                                                                              *
*******************************************************************************}

module pascaline1(output);

var mod1_i: integer;

procedure mod1_p;

begin

   writeln('This is the used module: ', mod1_i:1)

end;

virtual procedure abstract;

begin

   writeln('This is the abstract procedure')

end;

begin

end.
