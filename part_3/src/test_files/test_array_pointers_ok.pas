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

end.