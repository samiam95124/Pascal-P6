{

PRT test 1766: String compatibility with non-string.

    Assignment compatibility between string and non-string.

}

program iso7185prt1766(output);

type mychar = 'a'..'z';

var s: packed array [1..10] of mychar;

begin

   s := 'hello you ';
   writeln('The string is: ''', s, '''');

end.
