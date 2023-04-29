program TAC;


function f(x:integer; y:string; z:integer) : integer;
begin
   f := x;
end;


procedure p(x:integer; y:string; z:integer);
begin

end;

var 
   i : integer = 0;

const 
   x = 3;

begin
   // x is already decalred as a constant.
   function p(x:integer; y:string; z:integer) : integer;
   begin
      i := x; // There is not return.
   end;

   begin

      p(3, "prova", x);

   end;


end.
