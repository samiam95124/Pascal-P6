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

const second = 10_000;
                                                          
var sa, sb:     packed array 1000 of char;
    sp:         pstring;
    fla:        filptr;
    ta:         integer;
    ep:         envptr;
    i:          integer;
    atr:        attribute;
    ps:         pstring;
    p, n, e:    packed array 1000 of char;
    pp, pn, pe: pstring;
    ft:         text;
    sc:         schar;
    err:        integer;
    t:          integer;

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
   list('services_test.pas', fla);
   while fla <> nil do begin

      write(fla^.name^);
      fla := fla^.next

   end;
   writeln(' s/b services_test.pas');
   { this should not need to be more than 1 character, it is being evaluated
     as char }
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
   times(sa, local(time));
   writeln('test 5: ', sa:*, ' s/b <the current time in local>');
   dates(sa, local(time));
   writeln('test 7: ', sa:*, ' s/b <the current date>');
   write('test 9: ');
   writetime(output, local(time));
   writeln(' s/b <the time>');
   write('test 10: ');
   writedate(output, local(time));
   writeln(' s/b <the date>');
   ta := clock;
   writeln('test 11: ', elapsed(ta):1, ' s/b <a small amount of time>');
   writeln('test 12: ', validfile('/just/fargle.com'), ' s/b true');
   writeln('test 13: ', validfile(''), ' s/b false');
   writeln('test 14: ', wild('/fargle.c?m'), ' s/b true');
   writeln('test 15: ', validfile('/far*gle.com'), ' s/b true');
   writeln('test 16: ', validfile('      '), ' s/b false');
   writeln('test 17: ', wild('/for?.txt'), ' s/b true');
   writeln('test 18: ', wild('/for*.txt'), ' s/b true');
   writeln('test 19: ', wild('/fork.txt'), ' s/b false');
   setenv('barkbark', 'what is this');
   getenv('barkbark   ', sa);
   writeln('test20: ', sa:*, ' s/b what is this');
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
   t := clock;
   exec('services_test1');
   while elapsed(t) < second do;
   writeln('s/b This is services_test1: ''''');
   writeln('test25: ');
   execw('services_test1', err);
   writeln(err:1);
   writeln('s/b');
   writeln('This is services_test1: ''''');
   writeln('0');
   writeln('test26: ');
   new(ep);
   ep^.name := copy('bark');
   ep^.data := copy('hi there');
   ep^.next := nil;
   t := clock;
   exece('services_test1', ep);
   while elapsed(t) < second do;
   writeln('s/b This is services_test1: ''hi there''');
   writeln('test27: ');
   execew('services_test1', ep, err);
   writeln(err:1);
   writeln('s/b');
   writeln('This is services_test1: ''hi there''');
   writeln('0');
   getcur(sa);
   writeln('test 28: ', sa:*, ' s/b <the current path>');
   getcur(sb);
   setcur('/');
   getcur(sa);
   { same }
   writeln('test 30: ', sa:*, ' s/b /');
   setcur(sb);
   getcur(sa);
   writeln('test 31: ', sa:*, ' s/b <the current path>');
   brknam('/what/ho/junk.com', p, n, e);
   writeln('test 32: Path: ', p:*, ' Name: ', n:*, ' Ext: ', e:*);
   writeln('    s/b: Path: /what/ho/ Name: junk Ext: com');
   maknam(sa, p, n, e);
   writeln('test 33: ', sa:*, ' s/b /what/ho/junk.com');
   copyp(sa, 'junk');
   fulnam(sa);
   writeln('test 36: ', sa:*, ' s/b <path>junk');
   getpgm(sa);
   writeln('test 38: ', sa:*, ' s/b <the program path>');
   getusr(sa);
   writeln('test 40: ', sa:*, ' s/b <the user path>');
   assign(ft, 'junk');
   rewrite(ft);
   close(ft);
   { note: Unix cannot set attributes }
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
   { end of Unix test fails }
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
   write('test 50: ');
   filchr(sc);
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