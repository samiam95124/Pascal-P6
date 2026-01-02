{*******************************************************************************

Low level virtual memory interface for pint

Contains abstractions for accessing virtual memory.

*******************************************************************************}

module pint_mem(output);

uses mpb;  { machine parameter block }

const

maxstr      = 16777215; { maximum size of addressing for program/var }
maxtop      = 16777216; { maximum size of addressing for program/var+1 }
maxfil      = 100;      { maximum number of general (temp) files }

{ assigned logical channels for header files }
inputfn    = 1; { 'input' file no. }
outputfn   = 2; { 'output' file no. }
prdfn      = 3; { 'prd' file no. }
prrfn      = 4; { 'prr' file no. }
errorfn    = 5; { 'error' file no. }
listfn     = 6; { 'list' file no. }
commandfn  = 7; { 'command' file no. }

type

settype     = set of setlow..sethigh;
address     = -maxstr..maxtop; { address }
fileno      = 0..maxfil; { logical file number }
filsts      = (fnone, fclosed, fread, fwrite);

var

filtable    : array [1..maxfil] of text; { general (temp) text file holders }
filstate    : array [1..maxfil] of filsts;
newline     : boolean; { output is on new line }

{ Accessor functions

  These translate store variables to internal, and convert to and from store RAM
  formats.

  The acessors are fairly machine independent, they rely here on the machine
  being byte addressable. The endian format is inherent to the machine.

  The exception are the get/put int8,16,32,64 and 128 bit routines, which are
  dependent on the endian mode of the machine.

}

virtual function getint(a: address): integer;

begin

    refer(a);

    writeln('*** pint_mem: getint: abstract function not defined');
    halt;

    getint := 0

end;

virtual procedure putint(a: address; x: integer);

begin

    refer(a);
    refer(x);

    writeln('*** pint_mem: putint: abstract function not defined');
    halt

end;

virtual function getrel(a: address): real;

begin

    refer(a);

    writeln('*** pint_mem: getrel: abstract function not defined');
    halt;

    getrel := 1.0

end;

virtual procedure putrel(a: address; f: real);

begin

    refer(a);
    refer(f);

    writeln('*** pint_mem: putrel: abstract function not defined');
    halt

end;

virtual function getbol(a: address): boolean;

begin

    refer(a);

    writeln('*** pint_mem: getbol: abstract function not defined');
    halt;

    getbol := false

end;

virtual procedure putbol(a: address; b: boolean);

begin

    refer(a);
    refer(b);

    writeln('*** pint_mem: putbol: abstract function not defined');
    halt

end;

virtual procedure getset(a: address; var s: settype);

begin

    refer(a);
    refer(s);

    writeln('*** pint_mem: getset: abstract function not defined');
    halt

end;

virtual procedure putset(a: address; s: settype);

begin

    refer(a);
    refer(s);

    writeln('*** pint_mem: putset: abstract function not defined');
    halt

end;

virtual function getchr(a: address): char;

begin

    refer(a);

    writeln('*** pint_mem: getchr: abstract function not defined');
    halt;

    getchr := ' '

end;

virtual procedure putchr(a: address; c: char);

begin

    refer(a);
    refer(c);

    writeln('*** pint_mem: putchr: abstract function not defined');
    halt

end;

virtual function getbyt(a: address): byte;

begin

    refer(a);

    writeln('*** pint_mem: getbyt: abstract function not defined');
    halt;

    getbyt := 0

end;

virtual procedure putbyt(a: address; b: byte);

begin

    refer(a);
    refer(b);

    writeln('*** pint_mem: putbyt: abstract function not defined');
    halt

end;

virtual function getadr(a: address): address;

begin

    refer(a);

    writeln('*** pint_mem: getadr: abstract function not defined');
    halt;

    getadr := 0

end;

virtual procedure putadr(a: address; ad: address);

begin

    refer(a);
    refer(ad);

    writeln('*** pint_mem: putadr: abstract function not defined');
    halt

end;

virtual procedure newspc(len: address; var blk: address);

begin

    refer(len);
    refer(blk);

    writeln('*** pint_mem: newspc: abstract function not defined');
    halt

end;

virtual procedure valfil(fa: address);

begin

    refer(fa);

    writeln('*** pint_mem: valfil: abstract function not defined');
    halt

end;

virtual procedure errore(ei: integer);

begin

    refer(ei);

    writeln('*** pint_mem: valfil: abstract function not defined');
    halt

end;

begin
end.