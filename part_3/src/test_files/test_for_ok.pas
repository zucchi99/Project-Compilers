program forLoop;
var
   i,b: integer;
begin

   i := 0;
   
   for i := 10 to 20 do
   begin
      b := b + i;
      continue;
      break;
      b := i;
   end;

   writeInt(i);


end.