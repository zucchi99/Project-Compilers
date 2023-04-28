program Hello;

var 
x : integer;
q : char;
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
        q := 'q';
    end;

    procedure g(a,x : integer); begin end;
    
    begin 
        //x := 0;
        //g();
        z[1+2] := p(q,x,z[1]);
        g(q,x);
        //x := p();
    end;
    //a := p();
end.