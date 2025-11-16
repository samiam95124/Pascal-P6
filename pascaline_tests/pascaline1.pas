{$list-}
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

{ overloads }

procedure zork;

begin

   writeln('This is zork')

end;

overload procedure zork(i: integer);

begin

   writeln('zork: the integer is: ', i:1)

end;

overload procedure zork(s: string);

begin

   writeln('zork: the string is: ', s)

end;

overload function zork: integer;

begin

   zork := 42

end;

begin

    mod1_i := 42

end.
