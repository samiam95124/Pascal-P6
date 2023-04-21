{******************************************************************************
*                                                                             *
*                         TESTS FOR EXTLIB                                    *
*                                                                             *
*                   Copyright (C) 2001 S. A. Moore                            *
*                                                                             *
* Runs a series of tests on Windows extlib. Extlib itself is not OS           *
* dependent, but to test many of its features requires OS dependent testing.  *
* However, most of the material is OS independent, so generating another OS   *
* version does not require much work.                                         *
*                                                                             *
******************************************************************************}

program exttst(output);

uses strlib,
     extlib;
                                                          
var sa:         packed array [1..40] of char;
    sp:         pstring;
    fla:        filptr;
    ta:         integer;
    ep:         envptr;
    i:          integer;
    atr:        attribute;
    ps:         pstring;
    p, n, e:    packed array [1..40] of char;
    pp, pn, pe: pstring;
    ft:         text;
    sc:         chrset;
    err:        integer;

begin

   write('test 1: ');
   list('exttst.pas', fla);
   while fla <> nil do begin

      write(fla^.name^);
      fla := fla^.next

   end;
   writeln(' s/b exttst.pas');
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
   writeln('test 3: ', sa:0, ' s/b <the current time in zulu>');
   sp := times(time);
   writeln('test 4: ', sp^, ' s/b <the current time in zulu>');
   times(sa, local(time));
   writeln('test 5: ', sa:0, ' s/b <the current time in local>');
   sp := times(local(time));
   writeln('test 6: ', sp^, ' s/b <the current time in local>');
   dates(sa, local(time));
   writeln('test 7: ', sa:0, ' s/b <the current date>');
   sp := dates(local(time));
   writeln('test 8: ', sp^, ' s/b <the current date>');
   write('test 9: ');
   writetime(output, local(time));
   writeln(' s/b <the time>');
   write('test 10: ');
   writedate(output, local(time));
   writeln(' s/b <the date>');
   ta := clock;
   writeln('test 11: ', elapsed(ta):1, ' s/b <a small amount of time>');
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
   writeln('test20: ', sa:0, ' s/b what is this');
   sp := getenv('barkbark   ');
   writeln('test21: ', sp^, ' s/b what is this');
   remenv('barkbark');
   getenv('barkbark', sa);
   writeln('test22: ''', sa:0, ''' s/b ''''');
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
   exec('exttst1');
   writeln('waiting');
   writeln('waiting');
   writeln('waiting');
   writeln('waiting');
   writeln('waiting');
   writeln('waiting');
   writeln('waiting');
   writeln('waiting');
   writeln('waiting');
   writeln('waiting');
   writeln('waiting');
   writeln('waiting');
   writeln('waiting');
   writeln('waiting');
   writeln('waiting');
   writeln('waiting');
   writeln('waiting');
   writeln('waiting');
   writeln('waiting');
   writeln('waiting');
   writeln('waiting');
   writeln('waiting');
   writeln('waiting');
   writeln('s/b This is extst1: '''' (mixed with ''waiting'' lines above)');
   writeln('test25: ');
   execw('exttst1', err);
   writeln(err:1);
   writeln('s/b');
   writeln('This is extst1: ''''');
   writeln('0');
   writeln('test26: ');
   new(ep);
   ep^.name := copy('bark');
   ep^.data := copy('hi there');
   ep^.next := nil;
   exece('exttst1', ep);
   writeln('waiting');
   writeln('waiting');
   writeln('waiting');
   writeln('waiting');
   writeln('waiting');
   writeln('waiting');
   writeln('waiting');
   writeln('waiting');
   writeln('waiting');
   writeln('waiting');
   writeln('waiting');
   writeln('waiting');
   writeln('waiting');
   writeln('waiting');
   writeln('waiting');
   writeln('waiting');
   writeln('waiting');
   writeln('waiting');
   writeln('waiting');
   writeln('waiting');
   writeln('waiting');
   writeln('waiting');
   writeln('waiting');
   writeln('waiting');
   writeln('waiting');
   writeln('s/b This is extst1: ''hi there'' (mixed with ''waiting'' lines above)');
   writeln('test27: ');
   execew('exttst1', ep, err);
   writeln(err:1);
   writeln('s/b');
   writeln('This is extst1: ''hi there''');
   writeln('0');
   getcur(sa);
   writeln('test 28: ', sa:0, ' s/b <the current path>');
   sp := getcur;
   writeln('test 29: ', sp^:0, ' s/b <the current path>');
   ps := getcur;
   setcur('\\');
   getcur(sa);
   writeln('test 30: ', sa:0, ' s/b c:\\');
   setcur(ps^);
   getcur(sa);
   writeln('test 31: ', sa:0, ' s/b <the current path>');
   brknam('c:\\what\\ho\\junk.com', p, n, e);
   writeln('test 32: Path: ', p:0, ' Name: ', n:0, ' Ext: ', e:0);
   writeln('    s/b: Path: c:\\what\\ho\\ Name: junk Ext: com');
   maknam(sa, p, n, e);
   writeln('test 33: ', sa:0, ' s/b c:\\what\\ho\\junk.com');
   brknam('c:\\what\\ho\\junk.com', pp, pn, pe);
   writeln('test 34: Path: ', pp^, ' Name: ', pn^, ' Ext: ', pe^);
   writeln('    s/b: Path: c:\\what\\ho\\ Name: junk Ext: com');
   sp := maknam(pp^, pn^, pe^);
   writeln('test 35: ', sp^, ' s/b c:\\what\\ho\\junk.com');
   copy(sa, 'junk');
   fulnam(sa);
   writeln('test 36: ', sa:0, ' s/b <path>junk');
   sp := fulnam('trash');
   writeln('test 37: ', sp^, ' s/b <path>trash');
   getpgm(sa);
   writeln('test 38: ', sa:0, ' s/b <the program path>');
   sp := getpgm;
   writeln('test 39: ', sp^, ' s/b <the program path>');
   getusr(sa);
   writeln('test 40: ', sa:0, ' s/b <the user path>');
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
   write('test 51: ''', optchr, ''' s/b the option character');

   { tests remaining. See the C file }
   { pthchr }
   { latitude }
   { longitude }
   { altitude }
   { country }
   { countrys }
   { timezone }
   { daysave }
   { time24hour }
   { language }
   { languages }
   { decimal }
   { numbersep }
   { timeorder }
   { dateorder }
   { datesep }
   { timesep }
   { currchr }
   { seterr }

end.