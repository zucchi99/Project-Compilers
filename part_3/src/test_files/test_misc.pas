program Hello;
const x = 2.0;
var a, b : integer;
var j : integer;
procedure t(); forward;
procedure f(); 
    procedure g();
    var a : integer;
    begin 
        a := -1;
        // writeln(a);         
        // writeln('g'); 
    end; 
begin;
    g();
    // writeln('f');
end;
begin
  a := x + 2;
  x := 2;
  if (x > 2) then
  begin
  // writeln ("Hello World 2")
  end
  else
  begin
      // writeln(a);
      begin
          a := 4;
          f();
      end;
      // writeln(a);
  end;
  while (a > 0) do
  begin
      a := a - 1;
      // writeln(a);
  end;
end.

