{

                            Online Pascal Compiler.
                Code, Compile, Run and Debug Pascal program online.
Write your code in this editor and press "Run" button to execute it.
https://www.onlinegdb.com/online_pascal_compiler

}


program Hello;
const x = 2.0;
var a : integer;
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
  writeln ('Hello World 2');
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

