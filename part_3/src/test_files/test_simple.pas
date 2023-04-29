program Hello;

var x : integer = 1;
a : integer;
const a = 1; // Already defined
b = 2;

function p() : integer; begin end; // Return is missing

begin
    begin 
        x := 0;
        begin 
            x := 0;
        end
    end
end.