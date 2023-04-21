{******************************************************************************
*                                                                             *
*                         TESTS FOR EXTLIB                                    *
*                                                                             *
*                   Copyright (C) 2001 S. A. Moore                            *
*                                                                             *
******************************************************************************}

program exttst1(output);

uses extlib;

var p: pstring;

begin
  
   getenv('bark', p);
   writeln('This is exttst1: ''', p^, '''');

end.