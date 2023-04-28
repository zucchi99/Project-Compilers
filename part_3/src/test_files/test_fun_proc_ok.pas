program Hello;

var 
x : integer;
a : string;
z : array [1..10] of string;


begin
    function  p(a, x : integer; y : string) : string; 
    var c : char;
    begin 
        a := x;
        a := 1 + 2;
        p := "ciao";
        c := 'c';
    end;

    (*procedure g(); 
    begin 
    end;
    *)
    begin 
        //x := 0;
        //g();
        z[1+2] := p(x,x,z[1]);
        //x := p();
    end;
    //a := p();
end.