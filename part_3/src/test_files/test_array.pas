program exArrays;
var
   a : array [1..10, 1..1] of integer;
   z : array [3..5, 4..6] of integer;
   x : array [1..3, 2..4] of array [3..5, 4..6] of integer ;   (* n is an array of 10 integers *)
   i, j: integer;
   p : ^integer;

begin

   x[1,1] := z;
   // initialize elements of array n to 0
   for i := 1 to 10 do
      a[ i, 1 ] := i + 100;   (* set element at location i to i + 100 *)
      //output each array element's value       
      writeInt(a[i,1])
end.