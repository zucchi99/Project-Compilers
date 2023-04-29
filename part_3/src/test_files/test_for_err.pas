program forLoop;
var
   i,b: integer;
   c : real;
begin

   i := 0;
   
   // Normale ciclo
   for i := 10 to 20 do
   begin
      b := b + i;
      i := 0; // You can't do that
      continue;
      break;
   end;

   writeInt(i);

   // Dovrebbe uscire subito dal ciclo
   (*
   for a := 1 downto 5 do
   begin
      b := b * a;
   end;

   // Normale ciclo (step negativo)
   for a := 5 downto 1 do
   begin
      b := b * a;
   end;

   c := a + 4.4
   *)

end.