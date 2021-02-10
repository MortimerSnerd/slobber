(* Type representation for checking and symbol tables. The type conversion 
   functions assume constant expressions have already been evaluated to 
   terminal literals. (which is not true yet)  *)
MODULE Types;
IMPORT
   Ast, BinReader, BinWriter, Dbg, Lex:=Scanner, SYSTEM;

CONST
   (* Type kinds *)
   KByte*=0; KInteger*=1; KBoolean*=2; KReal*=3; KChar*=4; KSet*=5;
   KArray*=10; KPointer*=11; KRecord*=12; KTypeError*=13; KDeferredPtrTarget*=14;
   KProcedure*=15;KVoid*=16;KAny*=17;

   (* KAny - used for types for some builtin procedures, like LEN
      that are sort of generic *)
  

   (* type flags *)
   Export*=0;Var*=1;OpenArray*=2;Builtin*=3;FromLiteral*=4;

   MaxNameLen=64;
   PrimLookupLen=KAny;

   (* Version for the type format Write/Read can read *)
   BinFmtVer=200;

TYPE
   (* Extra information returned by type matching functions *)
   MatchCtx* = INTEGER;

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

   (* Associates a type with the node of a tree *)
   TypeNote* = POINTER TO TypeNoteDesc;
   TypeNoteDesc* = RECORD(Ast.Annotation)
      ty*: Type
   END;

VAR
   PrimNames:  ARRAY PrimLookupLen OF ARRAY 16 OF CHAR;
   PrimTyKinds: ARRAY PrimLookupLen OF INTEGER;
   PrimTyInstances: ARRAY PrimLookupLen OF Type;
      (* These three arrays associate primitive names to 
         type kind constants.  PrimTyInstances keep singletons
         of the primitive types we can return without having
         to create memory leaks when we create fresh ones 
         without a GC. *)

   (* singleton types we can return from type checking functions *)
   VoidType*, ErrorType*, ArrayChar2, BooleanType*, 
   CharLiteralType*, NilType*: Type;

PROCEDURE Note*(t: Ast.T; ty: Type); 
VAR n: TypeNote;
BEGIN
   NEW(n);
   n.ty := ty;
   Ast.Note(t, n)
END Note;
   
(* Returns the first type annotation attached to "t" or
   NIL if there is none *)
PROCEDURE Remember*(t: Ast.T): TypeNote;
VAR an: Ast.Annotation;
    rv: TypeNote;
BEGIN
   an := t.notes;
   WHILE (an # NIL) & ~(an IS TypeNote) DO
      an := an.anext
   END;
   IF an # NIL THEN
      rv := an(TypeNote)
   ELSE
      rv := NIL
   END;
   RETURN rv
END Remember;

PROCEDURE MkProcParam*(): ProcParam;
VAR rv: ProcParam;
BEGIN
   NEW(rv);
   rv.name := "";
   rv.ty := NIL;
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

(* The type, assuming tok is a type name like INTEGER *)
PROCEDURE LookupPrimitiveType*(scan: Lex.T; tok: Lex.Token): Type;
VAR i: INTEGER;
    rv: Type;
BEGIN
   rv := ErrorType;
   i := 0;
   WHILE (rv = ErrorType) & (i < LEN(PrimNames)) DO
      IF Lex.EqlString(scan, tok, PrimNames[i]) THEN
         rv := PrimTyInstances[i]
      ELSE
         INC(i)
      END
   END;
   RETURN rv
END LookupPrimitiveType;

(* Returns the type if the terminal is a literal of some kind *)
PROCEDURE TypeForTerminal*(tok: Lex.Token): Type;
VAR rv: Type;
    arty: ArrayType;
BEGIN
   (* NB - For single character strings, we treat them as characters.
           We still have to special case it in Equal() as occasionally
           that single char string was supposed to be a string, but
           that seems to be the less common case *)
   rv := ErrorType;
   IF (tok.kind = Lex.ConstInt) OR (tok.kind = Lex.ConstHex) THEN
      rv := PrimTyInstances[KInteger]
   ELSIF tok.kind = Lex.ConstReal THEN
      rv := PrimTyInstances[KReal]
   ELSIF tok.kind = Lex.ConstHexString THEN
      rv := CharLiteralType
   ELSIF tok.kind = Lex.ConstString THEN
       IF tok.len = 3 THEN
          rv := CharLiteralType
       ELSE
         (* TODO - this is a memory leak.  We should keep up  
            with these somewhere so they can be freed *)
          arty := MkArrayType();
          arty.ndims := 1;
          arty.dims[0] := tok.len - 2 + 1; (* +1 for null, -2 to ignore quotes *)
          arty.ty := PrimTyInstances[KChar];
          rv := arty
       END
   ELSIF tok.kind = Lex.KNIL THEN
      rv := NilType
   ELSIF (tok.kind = Lex.KTRUE) OR (tok.kind = Lex.KFALSE) THEN
      rv := PrimTyInstances[KBoolean]
   END;

   RETURN rv
END TypeForTerminal;


(* Return TyKind for primitive, or KTypeError if not recognized *)
PROCEDURE LookupPrimitive*(scan: Lex.T; tok: Lex.Token): INTEGER;
VAR ty: Type;
BEGIN
   ty := LookupPrimitiveType(scan, tok);
   RETURN ty.kind
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

PROCEDURE GetUnqualName*(t: Ast.Terminal; scan: Lex.T; VAR dest: QualName);
BEGIN
   dest.module[0] := 0X;
   Lex.Extract(scan, t.tok, dest.name)
END GetUnqualName;

(* Searches for a field with the given name. Searches base types too. 
   Returns NIL if none found. *)
PROCEDURE FindField*(rty: RecordType; scan: Lex.T; tok: Lex.Token): RecordField;
VAR rv: RecordField;
    done: BOOLEAN;
    recnext: Type;
BEGIN
   done := FALSE;
   WHILE ~done & (rty # NIL) DO
      rv := rty.fields;
      WHILE ~done & (rv # NIL) DO
         IF Lex.EqlString(scan, tok, rv.name) THEN 
            done := TRUE
         ELSE
            rv := rv.next
         END
      END;
      recnext := rty.base;
      IF recnext # NIL THEN rty := recnext(RecordType) ELSE rty := NIL END
   END;
   RETURN rv
END FindField; 


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
         Dbg.S(":"); Dbg.Ln;
         DbgPrint(pfld.ty, indent+2);
         pfld := pfld.next
      END
   END
END DbgPrint;

(* Skips any leading POINTER types, and returns the inner type *)
PROCEDURE DerefPointers*(ty: Type): Type;
BEGIN
   WHILE ty.kind = KPointer DO
      ty := ty(PointerType).ty
   END
   RETURN ty
END DerefPointers; 

(* Handles the fun case where we thought a single 
   char string literal was a character, but it was really 
   a string *)
PROCEDURE CharLiteralStringMatch(c, s: Type): BOOLEAN;
   RETURN (c.kind = KChar) & (FromLiteral IN c.flags) & 
          (s.kind = KArray) & (s(ArrayType).ty.kind = KChar)
END CharLiteralStringMatch;

(* Returns TRUE if the two types are equivalent 
   While KTypeError should never really get here, 
   we make sure to return false if one of the parameters
   is an error *)
PROCEDURE Equal*(a, b: Type): BOOLEAN;
VAR rv: BOOLEAN;
   art0, art1: ArrayType;
   rt0, rt1: RecordType;
   f0, f1: RecordField;
   pt0, pt1: ProcType;
   r0, r1: ProcParam;
   i: INTEGER;
BEGIN
   IF (a = NIL) & (b = NIL) THEN
      rv := TRUE
   ELSIF (a = NIL) OR (b = NIL) THEN
      rv := FALSE
   ELSIF (a.kind = KTypeError) OR (b.kind = KTypeError) THEN
      rv := FALSE
   ELSIF (a.kind = KAny) OR (b.kind = KAny) THEN
      rv := TRUE
   ELSIF a = b THEN
      rv := TRUE
   ELSIF CharLiteralStringMatch(a, b) OR CharLiteralStringMatch(b, a) THEN
      rv := TRUE
   ELSIF a.kind # b.kind THEN
      rv := FALSE
   ELSE
      IF a.kind = KArray THEN
         art0 := a(ArrayType);
         art1 := b(ArrayType);
         rv := (art0.ndims = art1.ndims) & Equal(art0.ty, art1.ty);
         IF rv THEN
            FOR i := 0 TO art0.ndims-1 DO
               IF art0.dims[i] # art1.dims[i] THEN
                  rv := FALSE
               END
            END
         END

      ELSIF a.kind = KPointer THEN
         rv := Equal(a(PointerType).ty, b(PointerType).ty)

      ELSIF a.kind = KRecord THEN
         (* Field order matters *)
         rt0 := a(RecordType);
         rt1 := b(RecordType);
         IF Equal(rt0.base, rt1.base) THEN
            f0 := rt0.fields;
            f1 := rt1.fields;
            rv := TRUE;
            WHILE rv & (f0 # NIL) & (f1 # NIL) DO
               IF ~Ast.StringEq(f0.name, f1.name) THEN
                  rv := FALSE
               ELSIF ~Equal(f0.ty, f1.ty) THEN
                  rv := FALSE
               ELSE
                  f0 := f0.next;
                  f1 := f1.next
               END
            END;
            IF rv & ((f0 # NIL) OR (f1 # NIL)) THEN
               (* different field count *)
               rv := FALSE
            END
         ELSE
            rv := FALSE
         END

      ELSIF a.kind = KProcedure THEN
         pt0 := a(ProcType);
         pt1 := b(ProcType);
         IF Equal(pt0.returnTy, pt1.returnTy) THEN
            r0 := pt0.params;
            r1 := pt1.params;
            rv := TRUE;
            WHILE rv & (r0 # NIL) & (r1 # NIL) DO
               (* param names don't matter *)
               rv := Equal(r0.ty,  r1.ty);
               r0 := r0.next;
               r1 := r1.next;
            END;
            IF rv & ((r0 # NIL) OR (r1 # NIL)) THEN
               (* different param count *)
               rv := FALSE
            END
         ELSE
            rv := FALSE
         END
      ELSE
         (* for all of the other primitive types, matching kinds
            is good enough *)
         rv := TRUE
      END
   END;
   RETURN rv
END Equal;

PROCEDURE IsRecord*(t: Type): BOOLEAN;
   RETURN t.kind = KRecord
END IsRecord;

PROCEDURE IsRecordRef*(t: Type): BOOLEAN;
   RETURN IsRecord(t) OR ((t.kind = KPointer) & (IsRecord(t(PointerType).ty)))
END IsRecordRef;

(* Is this an integer in general, regardless of width? *)
PROCEDURE IsInteger*(t: Type): BOOLEAN;
   RETURN t.kind = KInteger
END IsInteger;

PROCEDURE IsCharLiteral*(t: Type): BOOLEAN;
VAR rv: BOOLEAN;
    art: ArrayType;
BEGIN
   IF t.kind = KArray THEN
      art := t(ArrayType);
      IF (art.ndims = 1) & (art.dims[0] = 2) & (art.ty.kind = KChar) THEN
         rv := TRUE
      ELSE
         rv := FALSE
      END
   ELSE  
      rv := IsInteger(t)
   END
   RETURN rv
END IsCharLiteral;

PROCEDURE IsStructured*(t: Type): BOOLEAN;
   RETURN IsRecord(t) OR (t.kind = KArray)
END IsStructured;

(* Writes a binary version of the type that can be 
   read back later.
   TODO: do we only want to write the exported types? *)
PROCEDURE Write*(VAR w: BinWriter.T; ty: Type);
VAR i: INTEGER;
    art: ArrayType;
    rty: RecordType;
    rp: RecordField;
    pty: ProcType;
    pp: ProcParam;
BEGIN
   BinWriter.I32(w, BinFmtVer);
   BinWriter.I8(w, ty.kind);
   BinWriter.I32(w, SYSTEM.VAL(INTEGER, ty.flags));
   CASE ty.kind OF
   KByte..KSet, KVoid, KAny:
      (* Nothing else to do, the tag takes care of it *)
   |KArray:
      art := ty(ArrayType);
      BinWriter.I8(w, art.ndims);
      FOR i := 0 TO art.ndims-1 DO
         BinWriter.I32(w, art.dims[i])
      END;
      Write(w, art.ty)
   |KPointer:
      Write(w, ty(PointerType).ty)
   |KRecord:
      rty := ty(RecordType);
      IF BinWriter.Cond(w, rty.base # NIL) THEN
         Write(w, rty.base)
      END;
      rp := rty.fields;
      WHILE BinWriter.Cond(w, rp # NIL) DO
         BinWriter.String(w, rp.name);
         Write(w, rp.ty);
         BinWriter.Bool(w, rp.export);
         rp := rp.next
      END
   |KProcedure:
      pty := ty(ProcType);
      IF BinWriter.Cond(w, pty.returnTy # NIL) THEN
         Write(w, pty.returnTy)
      END;
      pp := pty.params;
      WHILE BinWriter.Cond(w, pp # NIL) DO
         BinWriter.String(w, pp.name);
         Write(w, pp.ty);
         pp := pp.next
      END
   END;
END Write;

PROCEDURE Read*(VAR r: BinReader.T): Type;
VAR i, kind, vers, flags: INTEGER;
    art: ArrayType;
    rty: RecordType;
    rp: RecordField;
    pty: ProcType;
    point: PointerType;
    pp: ProcParam;
    rv: Type;
BEGIN
   BinReader.I32(r, vers); ASSERT(vers = BinFmtVer);
   BinReader.I8(r, kind);
   BinReader.I32(r, flags);
   CASE kind OF
   KByte..KSet, KVoid, KAny:
      rv := MkPrim(kind);
   |KArray:
      art := MkArrayType();
      BinReader.I8(r, art.ndims);
      FOR i := 0 TO art.ndims-1 DO
         BinReader.I32(r, art.dims[i])
      END;
      art.ty := Read(r);
      rv := art
   |KPointer:
      point := MkPointerType();
      point.ty :=  Read(r);
      rv := point
   |KRecord:
      rty := MkRecordType();
      IF BinReader.Cond(r) THEN
         rty.base := Read(r)
      END;
      WHILE BinReader.Cond(r) DO
         rp := MkRecordField();
         BinReader.String(r, rp.name);
         rp.ty := Read(r);
         BinReader.Bool(r, rp.export);
         rp.next := rty.fields;
         rty.fields := rp
      END;
      RevRecordFieldList(rty.fields);
      rv := rty
   |KProcedure:
      pty := MkProcType();
      IF BinReader.Cond(r) THEN
         pty.returnTy := Read(r)
      END;
      WHILE BinReader.Cond(r) DO
         pp := MkProcParam();
         BinReader.String(r, pp.name);
         pp.ty := Read(r);
         pp.next := pty.params;
         pty.params := pp
      END;
      RevProcParam(pty.params);
      rv := pty
   END;
   rv.flags := SYSTEM.VAL(SET, flags);
   RETURN rv
END Read;

PROCEDURE SetupTables();
VAR arty: ArrayType;
    pty: PointerType;
   PROCEDURE PAssoc(i: INTEGER; n: ARRAY OF CHAR; k: INTEGER);
   BEGIN
      PrimNames[i] := n;
      PrimTyKinds[i] := k;
      PrimTyInstances[i] := MkPrim(k)
   END PAssoc;
BEGIN
   PAssoc(0, "BYTE", KByte);
   PAssoc(1, "INTEGER", KInteger);
   PAssoc(2, "BOOLEAN", KBoolean);
   PAssoc(3, "REAL", KReal);
   PAssoc(4, "CHAR", KChar);
   PAssoc(5, "SET", KSet);
   PAssoc(6, "ANY", KAny);

   VoidType := MkPrim(KVoid);
   ErrorType := MkPrim(KTypeError);
   arty := MkArrayType();
   arty.ndims := 1;
   arty.dims[0] := 2;
   arty.ty := PrimTyInstances[KChar];
   ArrayChar2 := arty;
   BooleanType := PrimTyInstances[KBoolean];
   CharLiteralType := MkPrim(KChar);
   CharLiteralType.flags := {FromLiteral};
   pty := MkPointerType();
   pty.ty := MkPrim(KAny);
   NilType := pty;
END SetupTables;

BEGIN
   SetupTables();
END Types.
