program ciao;

function p (var b: integer) : integer;
begin
    p := b^;
end;

var
    d: ^integer;
begin
    p(d);
end.