program forLoop;
var
   i,b: integer;

procedure p (var i : integer);
begin
   i^ := i^ - 1;
end;

begin

   i := 0;
   
   for i := 10 to 20 do
   begin
      b := b + i;
      continue;
      break;
      b := i;
      p(i@);
   end;

   writeInt(i);


end.