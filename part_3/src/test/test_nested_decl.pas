program Hello;

begin;
    procedure g(); begin end; // ok
    begin 
        g(); // ok
    end;
    g(); // error
end.