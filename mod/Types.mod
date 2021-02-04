(* Type representation for checking and symbol tables. The type conversion 
   functions assume constant expressions have already been evaluated to 
   terminal literals. (which is not true yet)  *)
MODULE Types;
IMPORT
   Ast, Dbg, Lex:=Scanner;

CONST
   (* Type kinds *)
   KByte*=0; KInteger*=1; KBoolean*=2; KReal*=3; KChar*=4; KSet*=5;
   KArray*=10; KPointer*=11; KRecord*=12; KTypeError*=13; KDeferredPtrTarget*=14;
   KProcedure*=15;

   (* type flags *)
   Export*=0; Var*=1;

   MaxNameLen=64;
   PrimLookupLen=6;

TYPE
   QualName* = RECORD
      module*, name*: ARRAY MaxNameLen OF CHAR
   END;

   (* Base class of all types.  For primitive types, this is
      all you need. *)
   Type* = POINTER TO TypeDesc;
   TypeDesc* = RECORD
      kind*: INTEGER;
      flags*: SET
   END;
   
   ArrayType* = POINTER TO ArrayTypeDesc;
   ArrayTypeDesc* = RECORD(TypeDesc)
      dims*: ARRAY 16 OF INTEGER;
      ndims*: INTEGER;
      ty*: Type
   END;

   PointerType* = POINTER TO PointerTypeDesc;
   PointerTypeDesc* = RECORD(TypeDesc)
      ty*: Type
   END;

   RecordField* = POINTER TO RecordFieldDesc;
   RecordFieldDesc* = RECORD
      name*: ARRAY MaxNameLen OF CHAR;
      ty*: Type;
      export*: BOOLEAN;
      next*: RecordField
   END;

   RecordType* = POINTER TO RecordTypeDesc;
   RecordTypeDesc* = RECORD(TypeDesc)
      base*: Type;
      fields*: RecordField
   END;

   ProcParam* = POINTER TO ProcParamDesc;
   ProcParamDesc* = RECORD
      (* not really needed, but helpful for debugging *)
      name*: ARRAY MaxNameLen OF CHAR;  
      ty*: Type;
      (* Number of open array wrappers to the type *)
      openArrays*: INTEGER;
      next*: ProcParam
   END;

   ProcType* = POINTER TO ProcTypeDesc;
   ProcTypeDesc* = RECORD(Type)
      returnTy*: Type;
      params*: ProcParam;
      (* Pointer to the procedure body, if the procedure is 
         in the current module. *)
      body*: Ast.Branch
   END;

   DeferredTarget* = POINTER TO DeferredTargetDesc;
   DeferredTargetDesc* = RECORD(Type)
      (* Reference in AST for this decl *)
      ast*: Ast.T;
      (* Name of record this is a placeholder for. *) 
      name*: ARRAY MaxNameLen OF CHAR
   END;

VAR
   PrimNames:  ARRAY PrimLookupLen OF ARRAY 16 OF CHAR;
   PrimTyKinds: ARRAY PrimLookupLen OF INTEGER;
      (* These two arrays associate primitive names to 
         type kind constants *)

PROCEDURE MkProcParam*(): ProcParam;
VAR rv: ProcParam;
BEGIN
   NEW(rv);
   rv.name := "";
   rv.ty := NIL;
   rv.openArrays := 0;
   rv.next := NIL;
   RETURN rv
END MkProcParam; 

PROCEDURE MkProcType*(): ProcType;
VAR rv: ProcType;
BEGIN
   NEW(rv);
   rv.kind := KProcedure;
   rv.flags := {};
   rv.returnTy := NIL;
   rv.params := NIL;
   rv.body := NIL;
   RETURN rv
END MkProcType;

PROCEDURE MkDeferredTarget*(): DeferredTarget;
VAR rv: DeferredTarget;
BEGIN
   NEW(rv);
   rv.kind := KDeferredPtrTarget;
   rv.flags := {};
   rv.name := "";
   rv.ast := NIL
   RETURN rv
END MkDeferredTarget; 

PROCEDURE MkRecordField*(): RecordField;
VAR rv: RecordField;
BEGIN
   NEW(rv);
   rv.name := "";
   rv.ty := NIL;
   rv.next := NIL;
   rv.export := FALSE;
   RETURN rv
END MkRecordField; 
   
PROCEDURE MkRecordType*(): RecordType;
VAR rv: RecordType;
BEGIN
   NEW(rv);
   rv.kind := KRecord;
   rv.flags := {};
   rv.base := NIL;
   rv.fields := NIL
   RETURN rv
END MkRecordType;

PROCEDURE MkPointerType*(): PointerType;
VAR rv: PointerType;
BEGIN
   NEW(rv);
   rv.kind := KPointer;
   rv.flags := {};
   rv.ty := NIL
   RETURN rv
END MkPointerType;

PROCEDURE MkArrayType*(): ArrayType;
VAR rv: ArrayType;
BEGIN
   NEW(rv);
   rv.kind := KArray;
   rv.flags := {};
   rv.ndims := 0;
   rv.ty := NIL
   RETURN rv
END MkArrayType;

PROCEDURE MkPrim*(kind: INTEGER): Type;
VAR rv: Type;
BEGIN
   NEW(rv);
   rv.kind := kind;
   rv.flags := {};
   RETURN rv
END MkPrim;

PROCEDURE RevRecordFieldList*(VAR l: RecordField);
VAR x, next, last: RecordField;
BEGIN
   IF l # NIL THEN
      last := NIL;
      x := l;
      WHILE x # NIL DO
         next := x.next;
         x.next := last;
         last := x;
         x := next
      END;
      l := last
   END
END RevRecordFieldList;

PROCEDURE RevProcParam*(VAR l: ProcParam);
VAR x, next, last: ProcParam;
BEGIN
   IF l # NIL THEN
      last := NIL;
      x := l;
      WHILE x # NIL DO
         next := x.next;
         x.next := last;
         last := x;
         x := next
      END;
      l := last
   END
END RevProcParam;

(* Return TyKind for primitive, or KTypeError if not recognized *)
PROCEDURE LookupPrimitive*(scan: Lex.T; tok: Lex.Token): INTEGER;
VAR i, rv: INTEGER;
BEGIN
   rv := KTypeError;
   i := 0;
   WHILE (rv = KTypeError) & (i < LEN(PrimNames)) DO
      IF Lex.EqlString(scan, tok, PrimNames[i]) THEN
         rv := PrimTyKinds[i]
      ELSE
         INC(i)
      END
   END;
   RETURN rv
END LookupPrimitive;

PROCEDURE IsQualified*(n: QualName): BOOLEAN;
   RETURN n.module[0] # 0X
END IsQualified;

PROCEDURE GetQualName*(b: Ast.Branch; scan: Lex.T; VAR dest: QualName);
VAR t: Ast.T;
BEGIN
   IF b.kind = Ast.BkQualIdent THEN
      t := Ast.GetChild(b, 0);
      IF t = NIL THEN
         dest.module := ""
      ELSE
         Lex.Extract(scan, t(Ast.Terminal).tok, dest.module);
      END;
      t := Ast.GetChild(b, 1);
      Lex.Extract(scan, t(Ast.Terminal).tok, dest.name)
   ELSE
      ASSERT(FALSE)
   END
END GetQualName;

(* Writes a pretty version of a type to the debug output *)
PROCEDURE DbgPrint*(t: Type; indent: INTEGER);
VAR i: INTEGER;
    art: ArrayType;
    pt: PointerType;
    rt: RecordType;
    fld: RecordField;
    pfld: ProcParam;
    dt: DeferredTarget;
    proc: ProcType;
BEGIN
   Dbg.Ind(indent);
   CASE t.kind OF
   KByte: Dbg.S("BYTE")
   |KInteger: Dbg.S("INTEGER")
   |KBoolean: Dbg.S("BOOLEAN")
   |KReal: Dbg.S("REAL")
   |KChar: Dbg.S("CHAR")
   |KSet: Dbg.S("SET")
   |KArray:
      Dbg.S("ARRAY ");
      art := t(ArrayType);
      FOR i := 0 TO art.ndims-1 DO
         Dbg.S(" ");
         Dbg.I(art.dims[i])
      END;
      Dbg.Ln; DbgPrint(art.ty, indent+1)
   |KPointer:
      Dbg.S("POINTER"); Dbg.Ln;
      pt := t(PointerType);
      DbgPrint(pt.ty, indent+1)
   |KRecord:
      Dbg.S("RECORD");
      rt := t(RecordType);
      IF rt.base # NIL THEN
         Dbg.S(" OF"); Dbg.Ln;
         DbgPrint(rt.base, indent+2)
      END;
      fld := rt.fields;
      WHILE fld # NIL DO
         Dbg.Ln; Dbg.Ind(indent+1);
         Dbg.S(fld.name);
         IF fld.export THEN Dbg.S("*") END;
         Dbg.Ln; DbgPrint(fld.ty, indent+2);
         fld := fld.next
      END
   |KTypeError:
      Dbg.S("ERROR");
   |KDeferredPtrTarget:
      dt := t(DeferredTarget);
      Dbg.S("DEFERRED RESOLVE: "); Dbg.S(dt.name)
   |KProcedure:
      Dbg.S("PROCEDURE");
      proc := t(ProcType);
      IF proc.returnTy # NIL THEN
         Dbg.Ln; Dbg.Ind(indent+1);
         Dbg.S("RETURNS"); Dbg.Ln;
         DbgPrint(proc.returnTy, indent+2)
      END;
      pfld := proc.params;
      WHILE pfld # NIL DO
         Dbg.Ln;Dbg.Ind(indent+1);
         IF Var IN pfld.ty.flags THEN
            Dbg.S("VAR ")
         END;
         Dbg.S(pfld.name);
         IF pfld.openArrays > 0 THEN
            Dbg.S(" ARRAY OF * "); Dbg.I(pfld.openArrays)
         END;
         Dbg.S(":"); Dbg.Ln;
         DbgPrint(pfld.ty, indent+2);
         pfld := pfld.next
      END
   END
END DbgPrint;
      
PROCEDURE SetupTables();
   PROCEDURE PAssoc(i: INTEGER; n: ARRAY OF CHAR; k: INTEGER);
   BEGIN
      PrimNames[i] := n;
      PrimTyKinds[i] := k;
   END PAssoc;
BEGIN
   PAssoc(0, "BYTE", KByte);
   PAssoc(1, "INTEGER", KInteger);
   PAssoc(2, "BOOLEAN", KBoolean);
   PAssoc(3, "REAL", KReal);
   PAssoc(4, "CHAR", KChar);
   PAssoc(5, "SET", KSet);
END SetupTables;

BEGIN
   SetupTables()
END Types.
