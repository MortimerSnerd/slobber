MODULE AMD64;
IMPORT 
   Target, Ty:=Types;

CONST
   PointerSize = 8;

VAR
   LayoutFwd: PROCEDURE(rty: Ty.RecordType);

PROCEDURE Alignment(t: Ty.Type): INTEGER;
VAR rv: INTEGER;
BEGIN
   CASE t.kind OF
   Ty.KByte, Ty.KChar:                    rv := 1
   |Ty.KInteger, Ty.KBoolean, Ty.KReal,
      Ty.KSet:                            rv := 4
   |Ty.KArray:                            rv := Alignment(t(Ty.ArrayType).ty)
   |Ty.KPointer..Ty.KPrimName:            rv := 8
   END
   RETURN rv
END Alignment;

PROCEDURE SizeOf*(ty: Ty.Type): INTEGER;
   (* Calculates the size of the given type.
      This could probably eventually be broken
      up into common pieces for the Type module
      and the bits where we need target knowledge *)
VAR rty: Ty.RecordType;
    aty: Ty.ArrayType;
    rv: INTEGER;
BEGIN
   rv := 0;
   CASE ty.kind OF
   Ty.KByte, Ty.KBoolean,Ty.KChar: 
      rv := 1
   |Ty.KInteger, Ty.KSet: 
      rv := 4   (* For now INTEGER 32 bit to match host compiler config *)
   |Ty.KReal: 
      rv := 4
   |Ty.KArray:
      aty := ty(Ty.ArrayType);
      IF Ty.OpenArray IN aty.flags THEN
         (* It gets passed as a pointer to to a record with then 
            element ptr and length *)
         rv := PointerSize
      ELSE
         rv := Target.Align(SizeOf(aty.ty), Alignment(aty.ty)) * aty.dim
      END
   |Ty.KPointer, Ty.KProcedure:
      rv := PointerSize;
   |Ty.KRecord:
      rty := ty(Ty.RecordType);
      LayoutFwd(rty);
      rv := rty.byteSize
   END;
   RETURN rv
END SizeOf;

PROCEDURE Layout(rty: Ty.RecordType);
VAR offset: INTEGER;
    rf: Ty.RecordField;
    base: Ty.RecordType;
BEGIN
   IF rty.byteSize = 0 THEN
      IF rty.base # NIL THEN
         base := rty.base(Ty.RecordType);
         Layout(base);
         offset := base.byteSize;
      ELSE
         offset := 0;
      END;
      rf := rty.fields;
      WHILE rf # NIL DO
         offset := Target.Align(offset, Alignment(rf.ty));
         rf.offset := offset;
         offset := offset + SizeOf(rf.ty);
         rf := rf.next
      END;
      rty.byteSize := offset;
   END
END Layout;

PROCEDURE Init*(VAR t: Target.T);
   (* Set up the "arch" part of the Target as X86_64 *) 
BEGIN
   t.arch.name := "X86_64";
   t.arch.Alignment := Alignment;
   t.arch.Layout := Layout;
   t.arch.SizeOf := SizeOf;
   t.arch.PointerSize := PointerSize;
END Init;

BEGIN
   LayoutFwd := Layout;
END AMD64.
