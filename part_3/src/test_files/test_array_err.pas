program exArrays;

var
   x :  array [0..3,0..1] of array [0..2] of integer ;
   p : ^array [0..3,0..1] of array [0..2] of integer ;
   i,j,k : integer;

begin

   // no copy di array
   //x := z;

   // use pointer of array
   p := x@;

   // initialize multi dimensional array
   for i := 0 to 3 do
      for j := 0 to 1 do
         for k := 0 to 2 do
         begin
            //x[i,j][k] := i + j + k;
            p^[i,j][k] := i + j + k;
            //writeInt(x[i,j][k])
         end;

end.