program Hello;

var x : integer;
    a : string;

begin
    function  p() : string; 
    begin 
        p:= "ciao";
    end;
    
    procedure g(); begin end;
    begin 
        x := 0;
        g();
        a := p();
        x:= p();
    end;

    // p is not visible here
    // declaration are visible only in the inner blocks, not in the same
    a := p(); 
end.