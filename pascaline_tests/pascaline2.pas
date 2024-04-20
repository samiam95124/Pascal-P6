{$list-}
{*******************************************************************************
*                                                                              *
*                        TEST JOINED MODULE FOR PASCALINE                      *
*                                                                              *
*            Copyright (C) 2008 S. A. Moore - All rights reserved              *
*                                                                              *
* Gives a sample module to be "joined" in the Pascaline main test.             *
* Note that this deliberately overlaps the names in pascaline1.pas.            *
*                                                                              *
*******************************************************************************}

module pascaline2(output);

var mod1_i: integer;

procedure mod1_p;

begin

   writeln('This is the joined module: ', mod1_i:1)

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

end.
