program repeatUntilLoop;
var
   a: integer;

begin
   a := 10;
   (* repeat until loop execution *)
   repeat
   begin
      a := a + 1
   end
   until a = 20;
end.