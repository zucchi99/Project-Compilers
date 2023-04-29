program ciao;

function p (a,b,c,d,e,f,g,h,i: integer; var x,y : ^integer ) : integer;
begin
    p := b;
    writeInt(x^);
    writeInt(y^);
end;

var
    a : integer = 10;
    b : ^integer;
begin
    b := a@;
    p(1,2,3,4,5,6,7,8,9, a@, b);
end.
