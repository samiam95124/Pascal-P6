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
* This test is written to be deterministic so it can run in the regression:   *
* it works inside the controlled fixture directory libs/tests/                *
* services_test_dir, installs a fixed environment, normalizes the machine-    *
* specific paths it would otherwise print, and does its file-creating tests   *
* in this program's own directory (unique per model, so parallel models do    *
* not collide). The time and date lines vary by definition and are matched    *
* by the gold standard with the dif '?' wildcard (testlibprog runs            *
* services_test through "dif -q").                                            *
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
    ps:         pstring;
    p, n, e:    packed array [1..100] of char;
    pp, pn, pe: pstring;
    ft:         text;
    sc:         schar;
    err:        integer;
    sts:        pstring; { absolute path of the exec helper }
    fgp:        pstring; { absolute path of the bin-resident getpgm helper }
    wkdir:      pstring; { this program's own directory (per-model scratch) }

procedure waittime(t: integer);

var ct: integer;

begin

    ct := clock;

    while elapsed(ct) < t do;

end;

{ List a single named file and print its directory entry. Used by the listing
  test to emit the fixture files in a fixed order (the library returns entries
  in raw directory order, which is not deterministic, so we name them). }

procedure prtfile(view fn: string);

var fl: filptr; atr: attribute;

begin

   list(fn, fl);
   if fl <> nil then begin

      { print the name without a field width: a negative (left-justify) field
        width is padded under pint/pmach but ignored under pgen/cmach, so a
        :-N here would make this line differ across backends }
      write(fl^.name^, ' ', fl^.size:-10, ' ', fl^.alloc:-10, ' ');
      for atr := atexec to atloop do if atr in fl^.attr then case atr of

         atexec: write('e');
         atarc:  write('a');
         atsys:  write('s');
         atdir:  write('d');
         atloop: write('l')

      end;
      writeln(' ')

   end

end;

begin

   { Resolve the exec helper to an absolute path while we are still at the repo
     root, remember this program's own directory as a per-model scratch area,
     install a fixed environment (so the environment dump and getusr are
     deterministic), then work inside the controlled fixture directory. }
   sts := fulnam('libs/services_test1');
   fgp := fulnam('bin/find_getpgm');
   wkdir := getpgm;
   allenv(ep);
   while ep <> nil do begin remenv(ep^.name^); ep := ep^.next end;
   setenv('home', '/home/testuser');
   setenv('alpha', 'one');
   setenv('beta', 'two');
   setenv('gamma', 'three');
   setcur('libs/tests/services_test_dir');

   write('test 1: ');
   list('alpha.txt', fla);
   while fla <> nil do begin

      write(fla^.name^);
      fla := fla^.next

   end;
   writeln(' s/b alpha.txt');
   writeln('test 2: ');
   writeln('Name                Size');
   writeln('========================');
   prtfile('alpha.txt');
   prtfile('beta.txt');
   prtfile('gamma.txt');
   writeln('s/b alpha.txt beta.txt gamma.txt (the fixture files)');
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
   writeln('test 11: ', abs(elapsed(ta)-second) <= second div 4, ' s/b true');
   writeln('test 12: ', validfile('/just/fargle.com'), ' s/b true');
   writeln('test 13: ', validfile('    '), ' s/b false');
   writeln('test 14: ', wild('fargle.c?m'), ' s/b true');
   writeln('test 15: ', validfile('c:\\far*gle.com'), ' s/b true');
   writeln('test 16: ', validfile(''), ' s/b false');
   writeln('test 17: ', wild('for?.txt'), ' s/b true');
   writeln('test 18: ', wild('/for*.txt'), ' s/b true');
   writeln('test 19: ', wild('fork.txt'), ' s/b false');
   setenv('barkbark', 'what is this');
   getenv('barkbark   ', sa);
   writeln('test20: ', sa:*, ' s/b what is this');
   sp := getenv('barkbark');
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
   writeln('s/b the controlled test environment');
   writeln('test24: ');
   exec(sts^);
   writeln('waiting 5 seconds for program to start');
   waittime(SECOND*5);
   writeln('s/b This is services_test1 "" (empty string)');
   writeln('test25: ');
   execw(sts^, err);
   writeln(err:1);
   writeln('s/b');
   writeln('This is services_test1: ''''');
   writeln('0');
   writeln('test26: ');
   new(ep);
   ep^.name := copy('bark');
   ep^.data := copy('hi there');
   ep^.next := nil;
   exece(sts^, ep);
   writeln('waiting 5 seconds');
   waittime(SECOND*5);
   writeln('s/b This is services_test1: "hi there"');
   writeln('s/b This is extst1: ''hi there'' (mixed with ''waiting'' lines above)');
   writeln('test27: ');
   execew(sts^, ep, err);
   writeln(err:1);
   writeln('s/b');
   writeln('This is extst1: ''hi there''');
   writeln('0');
   sp := getcur;
   brknam(sp^, p, n, e);
   writeln('test 28: ', n:*, ' s/b services_test_dir');
   sp := getcur;
   brknam(sp^, p, n, e);
   writeln('test 29: ', n:*, ' s/b services_test_dir');
   ps := getcur;
   setcur('/');
   getcur(sa);
   writeln('test 30: ', sa:*, ' s/b /');
   setcur(ps^);
   sp := getcur;
   brknam(sp^, p, n, e);
   writeln('test 31: ', n:*, ' s/b services_test_dir');
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
   brknam(sa, p, n, e);
   writeln('test 36: ', n:*, ' ', p[1] = '/', ' s/b junk true');
   sp := fulnam('trash');
   brknam(sp^, p, n, e);
   writeln('test 37: ', n:*, ' ', p[1] = '/', ' s/b trash true');
   { getpgm. This program's own binary is relocated into per-model directories
     by the parallel harness, so its own getpgm is not stable. Sub-execute the
     bin-resident helper find_getpgm, which prints its own getpgm basename; it
     always lives in bin, so it always reports "bin". }
   writeln('test 38: bin-resident helper getpgm:');
   execw(fgp^, err);
   writeln('test 39: helper exit ', err:1, ' s/b 0 (and "bin" above)');
   getusr(sa);
   writeln('test 40: ', sa:*, ' s/b /home/testuser');
   sp := getusr;
   writeln('test 41: ', sp^, ' s/b /home/testuser');

   { The file-creating tests run in this program's own directory, which is
     unique per execution model, so concurrent models in the regression do not
     collide on the scratch file/directory. }
   setcur(wkdir^);
   assign(ft, 'junk');
   rewrite(ft);
   close(ft);

   { Linux cannot set or reset attributes }
   {
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
   }

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
