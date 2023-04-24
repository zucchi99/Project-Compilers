program test_inside_block;

var 
    a : integer;
    c : string;
const 
    b = 1;

begin;
    while a < 10 do
    begin
        a := "ciao";
        b := 2;
        c := 2;
    end;

    repeat
    begin
        a := "ciao";
        b := 2;
        c := 2;
    end
    until a < 10;

    for a := 1 to 10 do
    begin
        a := "ciao";
        b := 2;
        c := 2;
    end;

    if a < 10 then
    begin
        a := "ciao";
        b := 2;
        c := 2;
    end
    else
    begin
        a := "ciao";
        b := 2;
        c := 2;
    end;
end.