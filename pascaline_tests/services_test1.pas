{******************************************************************************
*                                                                             *
*                         TESTS FOR SERVICES                                    *
*                                                                             *
*                   Copyright (C) 2001 S. A. Moore                            *
*                                                                             *
******************************************************************************}

program services_test1(output);

uses services;

var p: packed array 40 of char;

begin
  
   getenv('bark', p);
   writeln('This is exttst1: ''', p:*, '''');

end.