program Hello;
const x = 2.0;
var a, b : integer;
var j : integer;
procedure f(); 
    procedure g();
    var a : integer;
    begin 
        a := -1;
        writeln(a);         
        writeln('g'); 
    end; 
begin;
    g();
    writeln('f');
end;
begin
  a := 2;
  if x > 2 then
  begin
  writeln ("Hello World 2");
  end
  else
  begin
      writeln(a);
      begin
          a := 4;
          f();
      end;
      writeln(a);
  end;
  for j := 0 to 5 do
  begin
    writeln(j);
  end;
  for j := 0 to 5 do
  begin
    writeln(j);
  end
end.

