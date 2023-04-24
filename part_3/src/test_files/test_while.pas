program whileLoop;
var
   a: integer;
begin
   a := 10;
   while  a < 20  do
   begin
      a := a + 1;
      if a = 15 then
      begin
         a := a + 4;
         continue;
      end;
      a := a + 2;
   end;
   a := a + 3;
end.