program exArrays;
var
   x :  array [0..3] of integer ;   (* n is an array of 10 integers *)
   p : ^array [0..3] of integer ;   (* n is an array of 10 integers *)
   w : boolean;
begin

   // no copy di array
   //x := z;

   x[0] := 2;
   w := false;
   writeInt(x[0]);
   
   w := true;
   
   p := x@;
   w := false;
   p^[0] := 2;
   w := false;
   writeInt(p^[0]);

   (*writeInt(x[1,1][2]);
   writeInt(x[1,1][3]);
   writeInt(x[1,1][4]);
   writeInt(x[1,2][1]);
   writeInt(x[1,2][2]);
   writeInt(x[1,2][3]);
   writeInt(x[1,2][4]);
   writeInt(x[1,3][1]);
   writeInt(x[1,3][2]);
   writeInt(x[1,3][3]);
   writeInt(x[1,3][4]);
   writeInt(x[2,1][1]);
   writeInt(x[2,1][2]);
   writeInt(x[2,1][3]);
   writeInt(x[2,1][4]);
   writeInt(x[2,2][1]);
   writeInt(x[2,2][2]);
   writeInt(x[2,2][3]);
   writeInt(x[2,2][4]);
   writeInt(x[2,3][1]);
   writeInt(x[2,3][2]);
   writeInt(x[2,3][3]);
   writeInt(x[2,3][4]);*)

   // initialize elements of array n to 0
   (*
   for i := 1 to 10 do
      a[ i, 1 ] := i + 100;
      //output each array element's value       
      writeInt(a[i,1])
   *)
end.