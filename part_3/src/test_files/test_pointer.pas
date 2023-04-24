program exPointers;
var
   number: integer;
   x: ^integer;
   y: ^integer;

begin
   number := 100;
   
   x := number@;
   
   y^ := 200;

   x := y;

end.