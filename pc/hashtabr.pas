{*******************************************************************************
*                                                                              *
* Hash table generator                                                         *
*                                                                              *
* Generates tight chained tables for use with as. The output table is in the   *
* form of assignment to a 'ressym' array, suitable for direct insertion.       *
*                                                                              *
*******************************************************************************}

program hashtab(output);

uses strings; { string operations }

label 99;

const tabmax = 200; { maximum table length }
      labmax = 20;  { maximum label length }

type name = packed array[1..labmax] of char;
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

      read(infile, str[i]);
      if str[i] <> ' ' then i := i + 1

   end;
   readln(infile);
   if i > labmax then begin

      writeln('*** Error: input overflow');
      goto 99

   end

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

   assign(infile, 'restbl.lab');
   reset(infile);
   assign(outfile, 'restbl.pas');
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
   writeln(outfile, 'Reserved word hash table');
   writeln(outfile);
   writeln(outfile, '!!! DO NOT EDIT !!!');
   writeln(outfile);
   writeln(outfile, 'This file is autoamtically generated by hashtabr.');
   writeln(outfile);
   writeln(outfile, '*******************************************************************************}');
   writeln(outfile);
   writeln(outfile, 'module restbl;');
   writeln(outfile);
   writeln(outfile, 'uses strings, { string functions}');
   writeln(outfile, '     tolkens; { scanner tolkens }');
   writeln(outfile);
   { output entry count }
   writeln(outfile, 'const');
   writeln(outfile);
   writeln(outfile, '    resmax = ', mmax:1, ';');
   writeln(outfile, '    hashoff = ', madd:1, ';');
   writeln(outfile);
   writeln(outfile, 'type');
   writeln(outfile);
   writeln(outfile, '    resinx = 1..resmax; { index for reserved table }');
   writeln(outfile, '    resequ = record { reserved word table entry }');
   writeln(outfile);
   writeln(outfile, '                 lab:  pstring;  { reserved word }');
   writeln(outfile, '                 tolk: tolken;   { equivalent tolken }');
   writeln(outfile, '                 chn:  0..resmax { chain to next entry }');
   writeln(outfile); 
   writeln(outfile, '             end;');
   writeln(outfile);
   writeln(outfile, 'var');
   writeln(outfile);
   writeln(outfile, '    restbl: array [resinx] of resequ; { reserved words table }');
   writeln(outfile);

   writeln(outfile, 'begin');
   writeln(outfile);
   for i := 1 to mmax do begin { output result table }

      if ressym[i].reslab[1] = ' ' then begin { unfilled entry }

         write(outfile, '    restbl[', i:3, '].lab := copy(''', '<undefined>',
                     ''');');
         write(outfile, ' restbl[', i:3, '].chn := ', ressym[i].reschn:3, ';');
         writeln(outfile); { terminate line }
         write(outfile, '    restbl[', i:3, '].tolk := c', 'undefined', ';');
         writeln(outfile) { terminate line }

      end else begin

         write(outfile, '    restbl[', i:3, '].lab := copy(''', ressym[i].reslab:*,
                     ''');');
         write(outfile, ' restbl[', i:3, '].chn := ', ressym[i].reschn:3, ';');
         writeln(outfile); { terminate line }
         write(outfile, '    restbl[', i:3, '].tolk := c', ressym[i].reslab:*, ';');
         writeln(outfile) { terminate line }

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
