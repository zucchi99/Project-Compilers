program exArrays;

var
   x :  array [0..3,0..1] of array [0..2] of integer ;
   p : ^array [0..3,0..1] of array [0..1] of integer ;

begin

   p := x@;

end.