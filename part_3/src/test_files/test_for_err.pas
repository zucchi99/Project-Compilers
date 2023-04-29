program forLoop;
var
   i,b: integer;
begin

   i := 0;
   
   for i := 10 to 20 do
   begin
      b := b + i;
      i := 0; // You can't do that
      continue;
      break;
   end;

   writeInt(i);

end.