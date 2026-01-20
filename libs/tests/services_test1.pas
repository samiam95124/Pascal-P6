{******************************************************************************
*                                                                             *
*                         TESTS FOR EXTLIB                                    *
*                                                                             *
*                   Copyright (C) 2001 S. A. Moore                            *
*                                                                             *
******************************************************************************}

program services_test1(output);

uses services;

var p: pstring;

begin
  
   p := getenv('bark');
   writeln('This is services_test1: ''', p^, '''');

end.