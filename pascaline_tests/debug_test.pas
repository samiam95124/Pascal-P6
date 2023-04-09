program debug_test(output);

joins debug_test1;

var i: integer;
    c: char;
    b: boolean;
    e: (one, two, three);
    r: real;

begin

    writeln('This is debug test');
    debug_test1.writehi;
    i := 42;
    c := 'a';
    b := true;
    e := two;
    r := 12.34e10;

end.
