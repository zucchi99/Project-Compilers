program Hello;

var
    a,b : integer;
    c : boolean;
    d : string;
const 
    x="ciao";
begin
    begin 
        d := x;
        c := not ((x <> "false") and true); // The "not equal" operator is not allowed on string types
    end;
end.