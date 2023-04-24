program exArrays;
var
   //a : array [1..10, 1..1] of integer;
   //z : array [3..5, 4..6] of integer;
   x : array [0..3,0..2] of array [0..2] of integer ;   (* n is an array of 10 integers *)
   //i, j: integer;
   //p : ^array [1..2,1..3] of array [1..4] of integer ;   (* n is an array of 10 integers *)

begin

   writeInt(x[0,0][0]);
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