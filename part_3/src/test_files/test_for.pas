program forLoop;
var
   a,b: integer;
   c : real;
begin

   a := 0;
   
   // Normale ciclo
   for a := 10 to 20 do
   begin
      b := b + a;
      break;
   end;

   writeInt(a);

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