{ Stub program: references every network binding form so the mangled
  names appear in the object for trampoline generation. Compile with
  pc (the link fails; only the .o is needed). }
program networkstub(output);

uses network;

var ii: integer;
    lc: lcardinal;
    ss: packed array [1..100] of char;
    bb: packed array [1..100] of byte;
    tf, tg: text;
    cp: certptr;

begin
   addrnet(ss, lc);
   addrnetv6(ss, lc, lc);
   ii := maxmsg(lc);
   ii := maxmsgv6(lc, lc);
   ii := relymsg(lc);
   ii := relymsgv6(lc, lc);
   ii := openmsg(lc, 1, 1);
   ii := openmsgv6(lc, lc, 1, 1);
   wrmsg(1, bb);
   ii := rdmsg(1, bb);
   clsmsg(1);
   ii := waitmsg(1, 1);
   ii := certmsg(1, 1, ss);
   opennet(tf, tg, lc, 1, 0);
   opennetv6(tf, tg, lc, lc, 1, 0);
   waitnet(tf, tg, 1, 0);
   ii := certnet(tf, 1, ss);
   certlistnet(tf, 1, cp);
   certlistmsg(1, 1, cp);
   certlistfree(cp)
end.
