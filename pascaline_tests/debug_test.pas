program debug_test(output);

joins debug_test1;

type
    enum = (one, two, three);
    sub = 1..10;
    a10 = array 10 of integer;
    r = record i: integer; c: char; b: boolean; e: enum; sr: sub; r: real; 
               a: a10 end;
    r2 = record i: integer; c: char; b: boolean; e: enum; sr: sub; r: real; 
                rec: r; a: a10 end;
    a2 = array 10 of r2;

var i, x, i2: integer;
    c: char;
    b: boolean;
    e: enum;
    sr: sub;
    f: real;
    rv: r2;
    a: a10;
    ar: a2;

procedure proc1(i: integer; c: char; b: boolean; e: enum; sr: sub; r: real; rec: r2; a: a2);
begin
end;

begin

    writeln('This is debug test');
    debug_test1.writehi;
    x := 42;
    c := 'a';
    b := true;
    e := two;
    sr := 5;
    f := 12.34e10;

    rv.i := 12;
    rv.c := 'c';
    rv.b := false;
    rv.e := three;
    rv.sr := 6;
    rv.r := 56.78e20;
    rv.rec.i := 84;
    rv.rec.c := 'u';
    rv.rec.b := true;
    rv.rec.e := one;
    rv.rec.sr := 3;
    rv.rec.r := 54.32e8;
    for i := 1 to 10 do rv.rec.a[i] := i+20;
    for i := 1 to 10 do rv.a[i] := i+30;

    for i := 1 to 10 do a[i] := i+10;

    for i := 1 to 10 do begin

        ar[i].i := 12;
        ar[i].c := 'c';
        ar[i].b := false;
        ar[i].e := three;
        ar[i].sr := 1;
        ar[i].r := 56.78e20;

        ar[i].rec.i := 84;
        ar[i].rec.c := 'u';
        ar[i].rec.b := true;
        ar[i].rec.e := one;
        ar[i].rec.sr := 3;
        ar[i].rec.r := 54.32e8;
        for i2 := 1 to 10 do ar[i2].rec.a[i2] := i2+20;
        for i2 := 1 to 10 do ar[i2].a[i2] := i2+30;

    end;

    proc1(83, 'g', true, two, 7, 98.76, rv, ar);

end.
