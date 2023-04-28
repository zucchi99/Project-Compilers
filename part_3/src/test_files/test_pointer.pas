program exPointers;
var
   number: integer;
   x : ^integer;
   y : ^integer;
   z :  integer;

begin

   // NullAssignment
   number := 100;
   
   // ReadPointerAddress:  l  = @id (&id in C)
   x := number@;
   
   // WritePointerValue    ^l = r   (*id in C)
   y^ := 200;

   // ReadPointerValue:    l  = ^l  (*id in C)
   z := y^;
   

end.