program Hello;

procedure tryer(n:integer) ;
begin;
    n:=1
end;

// forward per singola passata del compilatore
function fact2(n:integer) : integer ; forward;

function factorial(n: integer; accumulator: integer): integer;
begin
  if n = 0 then
    factorial := accumulator
  else
    factorial := fact2(n-1);
end;

function fact2(n:integer) : integer;
begin 
    fact2 := factorial(n-1, 0);
end;

begin
    writeln(factorial(5, 1));
end.
