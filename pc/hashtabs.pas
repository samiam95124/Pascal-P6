{**************************************************************
*                                                             *
* Hash table generator                                        *
*                                                             *
* Generates tight chained tables for use with as.             *
* The output table is in the form of assignment to an         *
* 'ressym' array, suitable for direct insertion.              *
*                                                             *
**************************************************************}

program hashtab(input, output);

label 99;

const tabmax = 200; { maximum table length }
      labmax = 2;   { maximum label length }

type name = packed array[1..labmax] of char;
     byte = 0..255;
     labinx = 1..labmax; { index for label }

var loadarr, loadarrs: array [1..tabmax] of name; { holding table }
    ressym: array [1..tabmax] of { label table }
               record
                  reslab: name;
                  reschn: 0..tabmax { chain }
               end;
    max, maxt, mmax: 0..65535; { maximum number of entries }
    s : name;
    i, x, h: 0..tabmax; { indexes }
    lc: byte; { character on line count }
    infile, outfile: text; { input and output files }
    prime, mprime: integer; { number of prime entries }
    add, madd: integer;
    chncnt: array [1..10] of integer; { chain counts }
    ci: 1..10; { chain index }
    mccnt: integer; { minimum chain count }

procedure clrlab(var n: name);

var i: labinx;

begin

   for i := 1 to labmax do n[i] := ' '

end;

procedure inpstr(var str : name);

var i : integer;

begin

   clrlab(str);
   i := 1;
   while (i <= labmax) and not eoln(infile) do begin

      if i > labmax then begin

         writeln('*** Error: input overflow');
         goto 99

      end;
      read(infile, str[i]);
      if str[i] <> ' ' then i := i + 1

   end;
   readln(infile)

end;

function hash(s: name; add: integer; max: integer): byte;

var i, r : integer;

begin

   r := 0;
   for i := 1 to labmax do
      if s[i] <> ' ' then r := r + ord(s[i]) + add;
   hash := r mod max + 1

end;

{ process laying of entries to table }

procedure laydown(add: integer; max: integer);

var i, x, h: 0..tabmax; { indexes }

begin

   for i := 1 to tabmax do begin { clear result table }

      clrlab(ressym[i].reslab);
      ressym[i].reschn := 0

   end;
   prime := 0; { clear primes counter }
   for i := 1 to max do begin { find primes }

     if loadarr[i][1] <> ' ' then begin { occupied }

         h := hash(loadarr[i], add, max); { find the entry hash }
         if ressym[h].reslab[1] = ' ' then begin { prime }

            ressym[h].reslab := loadarr[i]; { place }
            clrlab(loadarr[i]); { clear old entry }
            prime := prime + 1

         end

      end

   end;
   for i := 1 to max do begin { find slots for non-prime }

      if loadarr[i][1] <> ' ' then begin { lefttover }

         h := hash(loadarr[i], add, max); { find the entry hash }
         { find terminal entry }
         while ressym[h].reschn <> 0 do h := ressym[h].reschn;
         x := 1; { find free slot }
         while ressym[x].reslab[1] <> ' ' do x := x + 1;
         ressym[h].reschn := x; { chain to this entry }
         ressym[x].reslab := loadarr[i]; { place entry }
         clrlab(loadarr[i]) { clear that }

      end

   end

end;

procedure count(max: integer);

var ci:   1..10; { chain counters index }
    cc:   integer; { chain counter }
    i, x: 0..tabmax; { indexes }

begin

   for ci := 1 to 10 do chncnt[ci] := 0; { clear chain counters }
   for i := 1 to max do begin

      cc := 0; { zero counter }
      x := i; { save index }
      while ressym[x].reschn <> 0 do begin { traverse }
      
         x := ressym[x].reschn; { chain }
         cc := cc + 1 { count }

      end;
      if cc <> 0 then begin { count chains }

         if cc >= 10 then chncnt[10] := chncnt[10] + 1
         else chncnt[cc] := chncnt[cc]+1

      end
   
   end

end;   

begin { hash }

   assign(infile, 'spctbl.lab');
   reset(infile);
   assign(outfile, 'spctbl.pas');
   rewrite(outfile);
   for i := 1 to tabmax do { clear save table }
      clrlab(loadarr[i]);
   i := 1; { index table start }
   while not eof(infile) do begin { read labels }

      inpstr(s);
      if i = tabmax then begin { overflow }

         writeln('Maximum number of entries exceeded');
         goto 99

      end;
      if s[1] <> ' ' then begin

         loadarr[i] := s; { place next symbol }
         i := i + 1 { next entry }

      end

   end;
   max := i-1; { set maximum number of entries }
   mprime := 0; { set minimum prime }
   mccnt := maxint; { set minimum chain count }
   for add := 0 to 100 do begin { hash offset tries }

      for maxt := max to max+10 do begin { max entry tries }

         loadarrs := loadarr; { save array }
         laydown(add, maxt); { laydown a set }
         count(maxt); { tally that }
         loadarr := loadarrs; { restore array }
{
         writeln('add: ', add:1, ' max = ', maxt: 1, ' prime = ', prime:1);
}
         if prime > mprime then 
	    begin madd := add; mprime := prime; mmax := maxt end
{         if chncnt[2] < mccnt then 
	    begin madd := add; mprime := prime; mmax := maxt; mccnt := chncnt[2] end
}

      end

   end;
   if mprime = 0 then begin 

      writeln('*** Error: no table solution found');
      goto 99

   end;
   laydown(madd, mmax); { process with minimum }
   count(mmax); { count the chains }
   writeln(outfile, '{*******************************************************************************');
   writeln(outfile);
   writeln(outfile, 'Symbol hash table');
   writeln(outfile);
   writeln(outfile, '!!! DO NOT EDIT !!!');
   writeln(outfile);
   writeln(outfile, 'This file is autoamatically generated by hashtabs.');
   writeln(outfile);
   writeln(outfile, '*******************************************************************************}');
   writeln(outfile);
   writeln(outfile, 'module spctbl;');
   writeln(outfile);
   writeln(outfile, 'uses tolkens; { scanner tolkens }');
   writeln(outfile);
   { output entry count }
   writeln(outfile, 'const');
   writeln(outfile);
   writeln(outfile, '    chrmax = ', mmax:1, '; { number of special character sequences }');
   writeln(outfile, '    chroff = ', madd:1, '; { special character hash offset }');
   writeln(outfile, '    spcmax = 2;  { special character string length }');
   writeln(outfile);
   writeln(outfile, 'type');
   writeln(outfile);
   writeln(outfile, '    chrinx = 1..chrmax; { special character index }');
   writeln(outfile, '    chrstr = packed array [1..spcmax] of char; { special character string }');
   writeln(outfile, '    chrequ = record { special character table entry }');
   writeln(outfile);
   writeln(outfile, '                lab:  chrstr;   { characters }');
   writeln(outfile, '                tolk: tolken;   { equivalent tolken }');
   writeln(outfile, '                chn:  0..chrmax { next entry chain }');
   writeln(outfile);
   writeln(outfile, '             end;');
   writeln(outfile);
   writeln(outfile, 'var');
   writeln(outfile);
   writeln(outfile, '    spctbl: array [chrinx] of chrequ; { special character table }');
   writeln(outfile);

   writeln(outfile, 'begin');
   writeln(outfile);
   for i := 1 to mmax do begin { output result table }

      if ressym[i].reslab[1] <> ' ' then begin { not empty }

         write(outfile, '    spctbl[', i:3, '].lab  := ''');
         for x := 1 to labmax do write(outfile, ressym[i].reslab[x]);
         write(outfile, ''';');
         if ressym[i].reschn <> 0 then { output chaining spec }
            write(outfile, ' spctbl[', i:3, '].chn := ',
                          ressym[i].reschn:3, ';');
         writeln(outfile); { terminate line }
         write(outfile, '    spctbl[', i:3, '].tolk := ');
         if ressym[i].reslab = '+ ' then write(outfile, 'cplus')
         else if ressym[i].reslab = '- ' then write(outfile, 'cminus')
         else if ressym[i].reslab = '* ' then write(outfile, 'ctimes')
         else if ressym[i].reslab = '/ ' then write(outfile, 'crdiv')
         else if ressym[i].reslab = '= ' then write(outfile, 'cequ')
         else if ressym[i].reslab = '<>' then write(outfile, 'cnequ')
         else if ressym[i].reslab = '><' then write(outfile, 'cnequa')
         else if ressym[i].reslab = '< ' then write(outfile, 'cltn')
         else if ressym[i].reslab = '> ' then write(outfile, 'cgtn')
         else if ressym[i].reslab = '<=' then write(outfile, 'clequ')
         else if ressym[i].reslab = '=<' then write(outfile, 'clequa')
         else if ressym[i].reslab = '>=' then write(outfile, 'cgequ')
         else if ressym[i].reslab = '=>' then write(outfile, 'cgequa')
         else if ressym[i].reslab = '( ' then write(outfile, 'clparen')
         else if ressym[i].reslab = ') ' then write(outfile, 'crparen')
         else if ressym[i].reslab = '[ ' then write(outfile, 'clbrkt')
         else if ressym[i].reslab = '(.' then write(outfile, 'clbrkt')
         else if ressym[i].reslab = '] ' then write(outfile, 'crbrkt')
         else if ressym[i].reslab = '.)' then write(outfile, 'crbrkt')
         else if ressym[i].reslab = '{ ' then write(outfile, 'clct')
         else if ressym[i].reslab = '(*' then write(outfile, 'clct')
         else if ressym[i].reslab = '} ' then write(outfile, 'crct')
         else if ressym[i].reslab = '*)' then write(outfile, 'crct')
         else if ressym[i].reslab = ':=' then write(outfile, 'cbcms')
         else if ressym[i].reslab = '. ' then write(outfile, 'cperiod')
         else if ressym[i].reslab = ', ' then write(outfile, 'ccma')
         else if ressym[i].reslab = '; ' then write(outfile, 'cscn')
         else if ressym[i].reslab = ': ' then write(outfile, 'ccln')
         else if ressym[i].reslab = '^ ' then write(outfile, 'ccmf')
         else if ressym[i].reslab = '@ ' then write(outfile, 'ccmf')
         else if ressym[i].reslab = '..' then write(outfile, 'crange')
         else if ressym[i].reslab = '# ' then write(outfile, 'cnum');
         write(outfile, ';');
         writeln(outfile) { terminate line }

      end else begin { empty }
 
         write(outfile, '    spctbl[', i:3, '].lab  := ''');
         for x := 1 to labmax do write(outfile, ressym[i].reslab[x]);
         write(outfile, ''';');
         if ressym[i].reschn <> 0 then { output chaining spec }
            write(outfile, ' spctbl[', i:3, '].chn := ',
                          ressym[i].reschn:3, ';');
         writeln(outfile); { terminate line }
         writeln(outfile, '    spctbl[', i:3, '].tolk := cundefined;');

      end

   end;
   writeln(outfile);
   writeln(outfile, 'end.');

   writeln('Number of entries processed: ', max:1);
   writeln('Number of prime entries: ', prime:1);
   writeln('Number of non-prime entries: ', max-prime:1);
   writeln('Add function: ', madd);
   writeln('Table pad: ', mmax-max);
   writeln('Chain counters:');
   writeln;
   for ci := 1 to 10 do { output chain counter table }
      writeln(ci:2, ': ', chncnt[ci]:2);
   writeln;
   writeln('Function complete');

   99:;

end.
