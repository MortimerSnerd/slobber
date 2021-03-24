(* Module implicitly imported by all modules, has runtime
   runtime types and functions.  These functions are
   not type safe to be called by users. *)
MODULE RUNTIME;
TYPE
   TYPEID* = POINTER TO VTABLE;
   VTABLEPTR* = POINTER TO VTABLE;
   VTABLEPTRPTR* = POINTER TO VTABLEPTR;
   VTABLE* = RECORD 
      parent*: VTABLEPTR
   END;

PROCEDURE HALT*(code: INTEGER);
   (* Stub function - halts the program with the given error code *)
BEGIN
END HALT;

PROCEDURE VTABLEFOR*(someRec: ANY): VTABLEPTR;
   (* Stub function.  Implemented by code generation *)
   RETURN NIL
END VTABLEFOR;

PROCEDURE ISTEST*(someRec: ANY; matchType: TYPEID): BOOLEAN;
   (* Implementation of IS test *)
VAR vt: VTABLEPTR;
    rv: BOOLEAN;
BEGIN
   vt := VTABLEFOR(someRec);
   rv := FALSE;
   WHILE ~rv & (vt # NIL) DO
      IF vt = matchType THEN
         rv := TRUE
      ELSE
         vt := vt^.parent
      END
   END;
   RETURN rv
END ISTEST;

(* 
TODOs
   - Specially handle VTABLEFOR in Il to generate the code/cast to get the
     vtable ptr for a record.
*)
BEGIN
END RUNTIME.
