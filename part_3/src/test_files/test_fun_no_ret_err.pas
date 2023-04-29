program Hello;
    
function f() : integer;
begin

    // f := something must be done at the upper level to avoid error
    // if done inside if/else/while/for/inner_blocks still error is given

    if true
    then
    begin
        f := 1;
    end
    else
    begin
        f := 2;
    end;

    while true do
        f := 3;

    begin
        f := -1;
    end;

end;

begin;
    f();
end.