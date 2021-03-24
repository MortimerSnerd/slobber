(* Has OS and arch information and methods *)
MODULE Target;
IMPORT
   Ty:=Types;

TYPE
   T* = POINTER TO RECORD
      arch*: RECORD
         name*: ARRAY 32 OF CHAR;
         SizeOf*: PROCEDURE(ty: Ty.Type): INTEGER;
            (* Returns the size of the given type in bytes *)
         Alignment*: PROCEDURE(t: Ty.Type): INTEGER;
            (* Alignment for a type *)
         Layout*: PROCEDURE(rty: Ty.RecordType);
            (* Populates offset and byteSize fields for the arch if
               not already done. *)
         PointerSize*: INTEGER
            (* Byte size of pointers *)
      END;
      os*: RECORD END
   END;

PROCEDURE MkBlankTarget*(): T;
VAR rv: T;
BEGIN
   NEW(rv);
   rv.arch.name := "UNKNOWN";
   RETURN rv
END MkBlankTarget;

PROCEDURE Align*(offset, alignment: INTEGER): INTEGER;
   (* Returns offset possibly incremented to align to 
      the given alignment *)
VAR r: INTEGER;
BEGIN
   (* NB - this sucks without explicit bitwise operations *)   
   r := offset MOD alignment; 
   IF r > 0 THEN
      offset := offset + (alignment - r)
   END;
   RETURN offset
END Align;


BEGIN
END Target.
