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

   function p(x:integer; y:string; z:integer) : integer;
   begin
      i := x;
   end;

   begin

      p(3, "prova", x);

   end;


end.


(*
program TAC;

//procedure p(); forward;

//procedure p (); begin end;

const x = 3;

begin

   //procedure p (); begin end;
   var
      x : integer = 5;
   begin
      x := 7;
      writeInt(x);
   end;

   writeInt(x);


   
end.
*)