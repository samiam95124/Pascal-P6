{******************************************************************************
*                                                                             *
*                         TESTS FOR SERVICE LIBRARY                           *
*                                                                             *
*                   Copyright (C) 2001 S. A. Moore                            *
*                                                                             *
* Runs a series of tests on services library. Services itself is not OS       *
* dependent, but to test many of its features requires OS dependent testing.  *
* However, most of the material is OS independent, so generating another OS   *
* version does not require much work.                                         *
*                                                                             *
******************************************************************************}

program services_test(output);

uses strings,
     services;

const second = 10000;
                                                          
var sa:         packed array [1..100] of char;
    sp:         pstring;
    fla:        filptr;
    ta:         integer;
    ep:         envptr;
    i:          integer;
    atr:        attribute;
    ps:         pstring;
    p, n, e:    packed array [1..100] of char;
    pp, pn, pe: pstring;
    ft:         text;
    sc:         schar;
    err:        integer;

procedure waittime(t: integer);

var ct: integer;

begin

    ct := clock;

    while elapsed(ct) < t do;

end;

begin

   write('test 1: ');
   list('INSTALL', fla);
   while fla <> nil do begin

      write(fla^.name^);
      fla := fla^.next

   end;
   writeln(' s/b INSTALL');
   list('*', fla);
   writeln('test 2: ');
   i := 10;
   writeln('Name                Size');
   writeln('========================');
   while (fla <> nil) and (i > 0) do begin

      write(fla^.name^:-20, ' ', fla^.size:-10, ' ', fla^.alloc:-10, ' ');
      for atr := atexec to atloop do if atr in fla^.attr then case atr of

         atexec: write('e');
         atarc:  write('a');
         atsys:  write('s');
         atdir:  write('d');
         atloop: write('l')
       
      end;
      writeln(' ');
      fla := fla^.next;
      i := i-1

   end;
   writeln('s/b <10 entries from the current directory>');
   times(sa, time);
   writeln('test 3: ', sa:*, ' s/b <the current time in zulu>');
   sp := times(time);
   writeln('test 4: ', sp^, ' s/b <the current time in zulu>');
   times(sa, local(time));
   writeln('test 5: ', sa:*, ' s/b <the current time in local>');
   sp := times(local(time));
   writeln('test 6: ', sp^, ' s/b <the current time in local>');
   dates(sa, local(time));
   writeln('test 7: ', sa:*, ' s/b <the current date>');
   sp := dates(local(time));
   writeln('test 8: ', sp^, ' s/b <the current date>');
   write('test 9: ');
   writetime(output, local(time));
   writeln(' s/b <the time>');
   write('test 10: ');
   writedate(output, local(time));
   writeln(' s/b <the date>');
   ta := clock;
   writeln('test11: waiting 1 second');
   waittime(SECOND);
   writeln('test 11: ', elapsed(ta), ' s/b ', second, ' (approximate)');
   writeln('test 12: ', validfile('c:\\just\\fargle.com'), ' s/b true');
   writeln('test 13: ', validfile('c:\\fargle.com'), ' s/b false');
   writeln('test 14: ', wild('c:\\fargle.c?m'), ' s/b true');
   writeln('test 15: ', validfile('c:\\far*gle.com'), ' s/b true');
   writeln('test 16: ', validfile('c:\\fargle.com'), ' s/b false');
   writeln('test 17: ', wild('c:\\for?.txt'), ' s/b true');
   writeln('test 18: ', wild('c:\\for*.txt'), ' s/b true');
   writeln('test 19: ', wild('c:\\fork.txt'), ' s/b false');
   setenv('barkbark', 'what is this');
   getenv('barkbark   ', sa);
   writeln('test20: ', sa:*, ' s/b what is this');
   sp := getenv('barkbark   ');
   writeln('test21: ', sp^, ' s/b what is this');
   remenv('barkbark');
   getenv('barkbark', sa);
   writeln('test22: ''', sa:*, ''' s/b ''''');
   allenv(ep);
   writeln('test23: ');
   i := 10;
   while (ep <> nil) and (i > 0) do begin

      writeln('Name: ''', ep^.name^, ''' Data: ''', ep^.data^, '''');
      ep := ep^.next;
      i := i-1

   end;
   writeln('s/b <10 entries from the current environment>');
   writeln('test24: ');
   exec('libs/services_test1');
   writeln('waiting 5 seconds for program to start');
   waittime(SECOND*5);
   writeln('s/b This is services_test1 "" (empty string)');
   writeln('test25: ');
   execw('libs/services_test1', err);
   writeln(err:1);
   writeln('s/b');
   writeln('This is services_test1: ''''');
   writeln('0');
   writeln('test26: ');
   new(ep);
   ep^.name := copy('bark');
   ep^.data := copy('hi there');
   ep^.next := nil;
   exece('libs/services_test1', ep);
   writeln('waiting 5 seconds');
   waittime(SECOND*5);
   writeln('s/b This is services_test1: "hi there"');
   writeln('s/b This is extst1: ''hi there'' (mixed with ''waiting'' lines above)');
   writeln('test27: ');
   execew('libs/services_test1', ep, err);
   writeln(err:1);
   writeln('s/b');
   writeln('This is extst1: ''hi there''');
   writeln('0');
   getcur(sa);
   writeln('test 28: ', sa:*, ' s/b <the current path>');
   sp := getcur;
   writeln('test 29: ', sp^:*, ' s/b <the current path>');
   ps := getcur;
   setcur('/');
   getcur(sa);
   writeln('test 30: ', sa:*, ' s/b /');
   setcur(ps^);
   getcur(sa);
   writeln('test 31: ', sa:*, ' s/b <the current path>');
   brknam('/what/ho/junk.com', p, n, e);
   writeln('test 32: Path: ', p:*, ' Name: ', n:*, ' Ext: ', e:*);
   writeln('    s/b: Path: /what/ho/ Name: junk Ext: com');
   maknam(sa, p, n, e);
   writeln('test 33: ', sa:*, ' s/b /what/ho/junk.com');
   brknam('/what/ho/junk.com', pp, pn, pe);
   writeln('test 34: Path: ', pp^, ' Name: ', pn^, ' Ext: ', pe^);
   writeln('    s/b: Path: /what/ho/ Name: junk Ext: com');
   sp := maknam(pp^, pn^, pe^);
   writeln('test 35: ', sp^, ' s/b c:\\what\\ho\\junk.com');
   copy(sa, 'junk');
   fulnam(sa);
   writeln('test 36: ', sa:*, ' s/b <path>junk');
   sp := fulnam('trash');
   writeln('test 37: ', sp^, ' s/b <path>trash');
   getpgm(sa);
   writeln('test 38: ', sa:*, ' s/b <the program path>');
   sp := getpgm;
   writeln('test 39: ', sp^, ' s/b <the program path>');
   getusr(sa);
   writeln('test 40: ', sa:*, ' s/b <the user path>');
   sp := getusr;
   writeln('test 41: ', sp^, ' s/b <the user path>');
   assign(ft, 'junk');
   rewrite(ft);
   close(ft);
   write('test 42: ');
   setatr('junk', [atarc]);
   list('junk', fla);
   if fla <> nil then write(fla^.name^, ' ', atarc in fla^.attr);
   writeln(' s/b junk true');
   write('test 43: ');
   resatr('junk', [atarc]);
   list('junk', fla);
   if fla <> nil then write(fla^.name^, ' ', atarc in fla^.attr);
   writeln(' s/b junk false');
   write('test 44: ');
   setatr('junk', [atsys]);
   list('junk', fla);
   if fla <> nil then write(fla^.name^, ' ', atsys in fla^.attr);
   writeln(' s/b junk true');
   write('test 45: ');
   resatr('junk', [atsys]);
   list('junk', fla);
   if fla <> nil then write(fla^.name^, ' ', atsys in fla^.attr);
   writeln(' s/b junk false');
   write('test 46: ');
   setuper('junk', [pmwrite]);
   list('junk', fla);
   if fla <> nil then write(fla^.name^, ' ', pmwrite in fla^.user);
   writeln(' s/b junk true');
   write('test 47: ');
   resuper('junk', [pmwrite]);
   list('junk', fla);
   if fla <> nil then write(fla^.name^, ' ', pmwrite in fla^.user);
   writeln(' s/b junk false');
   setuper('junk', [pmwrite]);
   delete('junk');
   write('test 48: ');
   makpth('junk');
   list('junk', fla);
   if fla <> nil then write(fla^.name^, ' ', atdir in fla^.attr);
   writeln(' s/b junk true');
   write('test 49: ');
   rempth('junk');
   list('junk', fla);
   writeln(fla = nil, ' s/b true');
   filchr(sc);
   write('test 50: ');
   for i := 0 to 255 do if chr(i) in sc then write(chr(i));
   writeln(' s/b <the set of valid characters>');
   writeln('test 51: ''', optchr, ''' s/b the option character');

end.