(* Stubs for the SYSTEM procedures that can be represented
   in the type system, used to make a symbol table *)
MODULE SYSTEM;

PROCEDURE BIT*(a, b: INTEGER): BOOLEAN;
   RETURN FALSE
END BIT;

PROCEDURE VAL*(t: PRIMTYNAME; v: ANY): ANY;
   (* Not accurate, semcheck has to special case
      this to make sure it really type checks *)
BEGIN
   RETURN v
END VAL;

BEGIN
END SYSTEM.
