program debug_test(output);

joins debug_test1;

type
    enum = (one, two, three);
    r = record i: integer; c: char; b: boolean; e: enum; r: real end;

var i, x: integer;
    c: char;
    b: boolean;
    e: enum;
    f: real;
    rv: r;
    a: array 10 of integer;
    ar: array 10 of r;

begin

    writeln('This is debug test');
    debug_test1.writehi;
    x := 42;
    c := 'a';
    b := true;
    e := two;
    f := 12.34e10;

    rv.i := 12;
    rv.c := 'c';
    rv.b := false;
    rv.e := three;
    rv.r := 56.78e20;

    for i := 1 to 10 do a[i] := i+10;

    for i := 1 to 10 do begin

        ar[i].i := 12;
        ar[i].c := 'c';
        ar[i].b := false;
        ar[i].e := three;
        ar[i].r := 56.78e20;

    end;

end.
