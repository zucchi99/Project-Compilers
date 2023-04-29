program Hello;
const x = 2.0;
var a : integer = 1;
var j : integer;
procedure f(); 
    procedure g();
    var a : integer;
    begin 
        a := -1;
        writeInt(a);         
        writeChar('g'); 
        f();
    end; 
begin;
    g();
    writeChar('f');
end;
begin
  a := 2;
  if x > 2 then
  begin
  writeString ("Hello World 2");
  end
  else
  begin
      writeInt(a);
      begin
          a := 4;
          f();
      end;
      writeInt(a);
  end;
  for j := 0 to 5 do
  begin
    writeInt(j);
  end;
  for j := 0 to 5 do
  begin
    j := 4; // Error, because j is read-only inside the loop
  end
end.

