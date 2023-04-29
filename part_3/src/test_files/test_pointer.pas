program exPointers;
var
   number: integer;
   x : ^integer;
   y : ^integer;
   z :  integer;

begin

   // NullAssignment
   number := 100;
   
   // ReadPointerAddress:  tmp  = @id (&id in C)
   // NullAssignment       l    = tmp 
   x := number@;
   
   // WritePointerValue    ^l   = r   (*id in C)
   y^ := 200;

   // ReadPointerValue:    tmp  = ^l  (*id in C)
   // NullAssignment       l    = tmp 
   z := y^;

   // ReadPointerValue:    tmp  = ^l  (*id in C)
   // WritePointerValue    ^l   = tmp (*id in C)
   x^ := y^;
   

end.