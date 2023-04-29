program costants;

const
   a = 2;
   x = "ciao";

var
   y: integer;
   z: string;

begin
   
   // it is not possible to redeclare a constant, neither in an inner-block
   var
      x : integer;

   const
      x = "prova";

   begin
   end;
   
end.