{

PRT test 1736: For round(x), if x is positive or zero then round(x) is
               equivalent to trunc(x+0.5), otherwise round(x) is equivalent to
               trunc(x- 0.5). It is an error if such a value does not exist.

               ISO 7185 reference: 6.6.5.3

}

program iso7185prt1736(output);

var a: integer;
    b: real;

begin

   { assign maximum value of integer }
   b := maxint;
   { now move it completely out of range in floating point only }
   b := b+10000.0;
   { now the assignment is invalid }
   a := round(b)

end.
