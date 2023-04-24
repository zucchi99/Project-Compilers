program exPointers;
var
   number: integer;
   iptr: ^integer;

begin
   number := 100;
   
   iptr := number@;

   iptr@ := 200;
   
   iptr^ := 200;
end.