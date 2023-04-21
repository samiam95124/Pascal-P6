{******************************************************************************
*                                                                             *
*                         TESTS FOR SERVICES                                  *
*                                                                             *
*                   Copyright (C) 2001 S. A. Moore                            *
*                                                                             *
* Runs a series of tests on Windows extlib. Extlib itself is not OS           *
* dependent, but to test many of its features requires OS dependent testing.  *
* However, most of the material is OS independent, so generating another OS   *
* version does not require much work.                                         *
*                                                                             *
******************************************************************************}

program services_test(output);

uses services;
                                                          
var sa, sb:     packed array 40 of char;
    sp:         pstring;
    fla:        filptr;
    ta:         integer;
    ep:         envptr;
    i:          integer;
    atr:        attribute;
    ps:         pstring;
    p, n, e:    packed array 40 of char;
    pp, pn, pe: pstring;
    ft:         text;
    sc:         schar;
    err:        integer;

function copy(view s: string) { source }
             : pstring; { result }

var d: pstring;

begin

   new(d, max(s)); { create destination }
   d^ := s; { copy string }
  
   result d { return result }

end;

procedure copyp(var d: string; view s: string);

var i: integer;

begin

   for i := 1 to max(d) do d[i] := ' ';
   for i := 1 to max(d) do if i <= max(s) then d[i] := s[i]

end;

begin

   write('test 1: ');
   list('exttst.pas', fla);
   while fla <> nil do begin

      write(fla^.name^);
      fla := fla^.next

   end;
   writeln(' s/b exttst.pas');
   { this should not need to be more than 1 character, it is being evaluated
     as char }
   list('* ', fla);
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
   times(sa, local(time));
   writeln('test 5: ', sa:0, ' s/b <the current time in local>');
   dates(sa, local(time));
   writeln('test 7: ', sa:0, ' s/b <the current date>');
   write('test 9: ');
   writetime(output, local(time));
   writeln(' s/b <the time>');
   write('test 10: ');
   writedate(output, local(time));
   writeln(' s/b <the date>');
   ta := clock;
   writeln('test 11: ', elapsed(ta):1, ' s/b <a small amount of time>');
   writeln('test 12: ', validfile('/just/fargle.com'), ' s/b true');
   writeln('test 13: ', validfile('/fargle.com'), ' s/b false');
   writeln('test 14: ', wild('/fargle.c?m'), ' s/b true');
   writeln('test 15: ', validfile('/far*gle.com'), ' s/b true');
   writeln('test 16: ', validfile('/fargle.com'), ' s/b false');
   writeln('test 17: ', wild('/for?.txt'), ' s/b true');
   writeln('test 18: ', wild('/for*.txt'), ' s/b true');
   writeln('test 19: ', wild('/fork.txt'), ' s/b false');
   setenv('barkbark', 'what is this');
   getenv('barkbark   ', sa);
   writeln('test20: ', sa:0, ' s/b what is this');
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
   getcur(sb);
   { this should produce \ }
   setcur('/ ');
   getcur(sa);
   { same }
   writeln('test 30: ', sa:0, ' s/b /');
   setcur(sb);
   getcur(sa);
   writeln('test 31: ', sa:0, ' s/b <the current path>');
   brknam('/what/ho/junk.com', p, n, e);
   writeln('test 32: Path: ', p:0, ' Name: ', n:0, ' Ext: ', e:0);
   writeln('    s/b: Path: /what/ho/ Name: junk Ext: com');
   maknam(sa, p, n, e);
   writeln('test 33: ', sa:0, ' s/b /what/ho/junk.com');
   copyp(sa, 'junk');
   fulnam(sa);
   writeln('test 36: ', sa:0, ' s/b <path>junk');
   getpgm(sa);
   writeln('test 38: ', sa:0, ' s/b <the program path>');
   getusr(sa);
   writeln('test 40: ', sa:0, ' s/b <the user path>');
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