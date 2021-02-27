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
   KProcedure*=15;KVoid*=16;KAny*=17;KNamedRef*=18;KPrimName*=19;
   NumTypeKinds=20;
      (* KAny - used for types for some builtin procedures, like LEN
         that are sort of generic.
         KNamedRef - used to break cycles when writing a type out
                     to a file.  Should not exist outside of 
                     the serialized form 
         KPrimName - for special SYSTEM functions that take a type name.
         *)
  

   (* type flags *)
   Export*=0;Var*=1;OpenArray*=2;Builtin*=3;FromLiteral*=4;
   Visited=5;
   Synthetic*=6;  (* Generated, do not save out to a file *)

   MaxNameLen=64;
   PrimLookupLen=NumTypeKinds;

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
      flags*: SET;
      srcName*: POINTER TO QualName
          (* The qualfied name of this type from the original src
            module.  Used for error reporting and short-cutting
            some type checks. If t1.srcName = t2.srcName, then
            you can assume the types are equal.  But 
            t1.srcName # t2.srcName does not mean the types
            are not equal.  This is because types can be aliased
            in a type section: TYPE t0=Mod.t4 *)
  END;
   
   (* Array type.  ARRAY 3, 4 OF CHAR == ARRAY 3 OF ARRAY 4 OF CHAR *)
   ArrayType* = POINTER TO ArrayTypeDesc;
   ArrayTypeDesc* = RECORD(TypeDesc)
      dim*: INTEGER;
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
      flags*: SET;
      next*: RecordField;
      offset*: INTEGER
         (* Byte offset into struct.  Not filled out here, 
            location depends on target arch *)
   END;

   RecordType* = POINTER TO RecordTypeDesc;
   RecordTypeDesc* = RECORD(TypeDesc)
      base*: Type;
      fields*: RecordField;
      byteSize*: INTEGER
         (* Not filled out here, but by the arch specific
            target.  We don't know without knowing 
            alignment restrictions *)
   END;

   ProcParam* = POINTER TO ProcParamDesc;
   ProcParamDesc* = RECORD
      (* not really needed, but helpful for debugging *)
      name*: ARRAY MaxNameLen OF CHAR;  
      seekpos*: INTEGER;
         (* seek position of the proc name in the source *)
      ty*: Type;
      next*: ProcParam
   END;

   ProcType* = POINTER TO ProcTypeDesc;
   ProcTypeDesc* = RECORD(TypeDesc)
      returnTy*: Type;
      params*: ProcParam;
      (* Pointer to the procedure body, if the procedure is 
         in the current module. *)
      body*: Ast.Branch
   END;

   DeferredTarget* = POINTER TO DeferredTargetDesc;
   DeferredTargetDesc* = RECORD(TypeDesc)
      (* Reference in AST for this decl *)
      ast*: Ast.T;
      (* Name of record this is a placeholder for. *) 
      name*: ARRAY MaxNameLen OF CHAR
   END;

   (* Associates a type with the node of a tree *)
   TypeNote* = POINTER TO TypeNoteDesc;
   TypeNoteDesc* = RECORD(Ast.AnnotationDesc)
      ty*: Type
   END;

   SSType = POINTER TO SSTypeDesc;
   SSTypeDesc = RECORD
      name: QualName;
      ty: Type;
      next: SSType
   END;

   SerialState* = RECORD
      (* State we use to break and reform loops when 
         writing/reading types from a file *)
      types: SSType
         (* Named type references *)
   END;

   (* Signature for function that calculates the
      required alignment for a type *)
   AlignFn* = PROCEDURE(t: Type): INTEGER;

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
   rv.seekpos := 0;
   RETURN rv
END MkProcParam; 

PROCEDURE MkProcType*(): ProcType;
VAR rv: ProcType;
BEGIN
   NEW(rv);
   rv.kind := KProcedure;
   rv.flags := {};
   rv.srcName := NIL;
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
   rv.srcName := NIL;
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
   rv.flags := {};
   rv.offset := 0;
   RETURN rv
END MkRecordField; 
   
PROCEDURE MkRecordType*(): RecordType;
VAR rv: RecordType;
BEGIN
   NEW(rv);
   rv.kind := KRecord;
   rv.flags := {};
   rv.srcName := NIL;
   rv.base := NIL;
   rv.fields := NIL;
   rv.byteSize := 0;
   RETURN rv
END MkRecordType;

PROCEDURE MkPointerType*(): PointerType;
VAR rv: PointerType;
BEGIN
   NEW(rv);
   rv.kind := KPointer;
   rv.flags := {};
   rv.srcName := NIL;
   rv.ty := NIL
   RETURN rv
END MkPointerType;

PROCEDURE MkArrayType*(): ArrayType;
VAR rv: ArrayType;
BEGIN
   NEW(rv);
   rv.kind := KArray;
   rv.flags := {};
   rv.srcName := NIL;
   rv.dim := 0;
   rv.ty := NIL
   RETURN rv
END MkArrayType;

PROCEDURE MkPrim*(kind: INTEGER): Type;
VAR rv: Type;
BEGIN
   NEW(rv);
   rv.kind := kind;
   rv.flags := {};
   rv.srcName := NIL;
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

(* The type, assuming tok is a type name like INTEGER.  Returns
   ErrorType if not found. *)
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

PROCEDURE IsIntConvertible*(kind: INTEGER): BOOLEAN;
   RETURN (kind = KInteger) OR (kind = KByte)
END IsIntConvertible;

(* There is some limited amount of primitive conversion
   that Oberon seems to allow for some of the primitive
   integer based types.  Check and see if the two types
   are allowed to be equal in the face of these conversions. 
   For constants, semcheck will complain about obvious
   data loss *)
PROCEDURE IntTypesCanCoerceEqual(a, b: Type): BOOLEAN;
   RETURN IsIntConvertible(a.kind) & IsIntConvertible(b.kind)
END IntTypesCanCoerceEqual;

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
          arty.dim := tok.len - 2 + 1; (* +1 for null, -2 to ignore quotes *)
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

PROCEDURE QNEqual*(a, b: QualName): BOOLEAN;
   RETURN Ast.StringEq(a.module, b.module) & Ast.StringEq(a.name, b.name)
END QNEqual;

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
      Dbg.S("ERROR/BUG NOT A QUALIDENT"); Dbg.Ln;
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

PROCEDURE FindFieldStr*(rty: RecordType; name: ARRAY OF CHAR): RecordField;
   (* Find a field by a string.  Use FindField() if you have a Lex.token *)
VAR rv, fld: RecordField; 
BEGIN
   rv := NIL;
   WHILE (rv = NIL) & (rty # NIL) DO
      fld := rty.fields;
      WHILE (rv = NIL) & (fld # NIL) DO
         IF Ast.StringEq(fld.name, name) THEN
            rv := fld
         ELSE
            fld := fld.next
         END
      END;
      IF rty.base = NIL THEN
         rty := NIL
      ELSE
         rty := rty.base(RecordType)
      END
   END
   RETURN rv
END FindFieldStr;    

(* Writes a short one line version of the type to the debug output, 
   preferring the name for the type if there is one *)
PROCEDURE DbgSummary*(t: Type);
VAR art: ArrayType;
    rty: RecordType;
    fld: RecordField;
    pfld: ProcParam;
    proc: ProcType;
BEGIN
   IF t.srcName # NIL THEN
      Dbg.S(t.srcName.module); Dbg.S(".");
      Dbg.S(t.srcName.name)
   ELSE
      CASE t.kind OF
      KByte: Dbg.S("BYTE")
      |KInteger: Dbg.S("INTEGER")
      |KBoolean: Dbg.S("BOOLEAN")
      |KReal: Dbg.S("REAL");
      |KChar: Dbg.S("CHAR"); 
      |KSet: Dbg.S("SET")
      |KPrimName: Dbg.S("PRIMTYNAME")
      |KVoid: Dbg.S("VOID")
      |KArray:
         art := t(ArrayType);
         Dbg.S("ARRAY ");
         IF ~(OpenArray IN t.flags) THEN
             Dbg.I(art.dim); Dbg.S(" ");
         END;
         Dbg.S("OF ");
         DbgSummary(art.ty)
      |KPointer:
         Dbg.S("POINTER TO ");
         DbgSummary(t(PointerType).ty)
      |KRecord:
         rty := t(RecordType);
         fld := rty.fields;
         Dbg.S("RECORD");
         IF rty.base # NIL THEN
            Dbg.S("("); DbgSummary(rty.base); Dbg.S(")")
         END;
         Dbg.S(" [");
         WHILE fld # NIL DO
            Dbg.S(" ");
            Dbg.S(fld.name);
            IF fld.next # NIL THEN Dbg.S(",") END;
            fld := fld.next
         END;
         Dbg.S("]")
      |KTypeError: Dbg.S("ERROR")
      |KDeferredPtrTarget: Dbg.S("DEFERRED PTR")
      |KProcedure: 
         Dbg.S("PROCEDURE");
         proc := t(ProcType);
         pfld := proc.params;
         IF pfld # NIL THEN
            Dbg.S("(");
            WHILE pfld # NIL DO
               DbgSummary(pfld.ty);
               IF pfld.next # NIL THEN Dbg.S(",") END;
               pfld := pfld.next
            END;
         END;
         IF proc.returnTy # NIL THEN
            Dbg.S(": ");
            DbgSummary(proc.returnTy)
         END
      END
   END
END DbgSummary;
         
(* Writes a pretty version of a type to the debug output *)
PROCEDURE DbgPrint*(t: Type; indent: INTEGER);
VAR art: ArrayType;
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
   |KPrimName: Dbg.S("PRIMTYNAME")
   |KArray:
      Dbg.S("ARRAY ");
      art := t(ArrayType);
      Dbg.I(art.dim);
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
         IF Export IN fld.flags THEN Dbg.S("*") END;
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
BEGIN
   IF (a = NIL) & (b = NIL) THEN
      rv := TRUE
   ELSIF (a = NIL) OR (b = NIL) THEN
      rv := FALSE
   ELSIF IntTypesCanCoerceEqual(a, b) THEN
      rv := TRUE
   ELSIF (a.kind = KTypeError) OR (b.kind = KTypeError) THEN
      rv := FALSE
   ELSIF (a.kind = KAny) OR (b.kind = KAny) THEN
      rv := TRUE
   ELSIF a = b THEN
      rv := TRUE
   ELSIF (a.srcName # NIL) & (b.srcName # NIL) 
            & QNEqual(a.srcName^, b.srcName^) THEN
      (* Having the same qualified type name is a shortcut for
         equality *)
      rv := TRUE
   ELSIF CharLiteralStringMatch(a, b) OR CharLiteralStringMatch(b, a) THEN
      rv := TRUE
   ELSIF a.kind # b.kind THEN
      rv := FALSE
   ELSE
      IF a.kind = KArray THEN
         art0 := a(ArrayType);
         art1 := b(ArrayType);
         rv := Equal(art0.ty, art1.ty);
         IF rv THEN
            IF ~(OpenArray IN art0.flags) & ~(OpenArray IN art1.flags) THEN
               rv := art0.dim = art1.dim
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

PROCEDURE IsSubtype*(a, b: Type): BOOLEAN;
   (* Returns true if a is a subtype of b *)
VAR rv: BOOLEAN;
BEGIN
   rv := FALSE;
   a := DerefPointers(a);
   b := DerefPointers(b);
   IF (a.kind = KRecord) & (b.kind = KRecord) THEN
      WHILE ~rv & (a # NIL) DO
         IF Equal(a, b) THEN
            rv := TRUE
         ELSE
            a := a(RecordType).base
         END
      END
   END;
   RETURN rv
END IsSubtype;

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
      IF (art.dim = 2) & (art.ty.kind = KChar) THEN
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

PROCEDURE IsPrimitive*(ty: Type): BOOLEAN;
   RETURN (ty#NIL) & (PrimTyInstances[ty.kind] # NIL) 
          & (ty.kind # KPrimName)
END IsPrimitive;

(* Prepares a SerialState to Read/Write type information *)
PROCEDURE InitSerialState*(VAR ss: SerialState);
BEGIN
   ss.types := NIL
END InitSerialState;

PROCEDURE RememberSerType(ss: SerialState; qn: QualName): SSType;
VAR rv: SSType;
BEGIN
   rv := ss.types;
   WHILE (rv # NIL) & ~QNEqual(qn, rv.name) DO
      rv := rv.next
   END;
   RETURN rv
END RememberSerType;

PROCEDURE NoteSerType(VAR ss: SerialState; ty: Type);
VAR st: SSType;
BEGIN
   IF ty.srcName # NIL THEN
      NEW(st);
      st.name := ty.srcName^;
      st.ty := ty;
      st.next := ss.types;
      ss.types := st
   END
END NoteSerType;
      
(* Writes a binary version of the type that can be 
   read back later.*)
PROCEDURE Write*(VAR w: BinWriter.T; VAR ss: SerialState; ty: Type);
VAR art: ArrayType;
    rty: RecordType;
    rp: RecordField;
    pty: ProcType;
    pp: ProcParam;
    old: POINTER TO QualName;
BEGIN
   IF (ty.srcName # NIL) & (RememberSerType(ss, ty.srcName^) # NIL) THEN
      (* We've already written this named type out, so now replace
         further refrences with just a reference by name *)
      old := ty.srcName;
      ty := MkPrim(KNamedRef);
      ty.srcName := old
   ELSE
      NoteSerType(ss, ty)
   END;
   EXCL(ty.flags, Visited);
   BinWriter.I32(w, BinFmtVer);
   BinWriter.I8(w, ty.kind);
   BinWriter.I32(w, SYSTEM.VAL(INTEGER, ty.flags));
   IF BinWriter.Cond(w, ty.srcName # NIL) THEN
      BinWriter.String(w, ty.srcName.module);
      BinWriter.String(w, ty.srcName.name)
   END;
   CASE ty.kind OF
   KByte..KSet, KVoid, KAny,KNamedRef,KPrimName:
      (* Nothing else to do, the tag takes care of it *)
   |KArray:
      art := ty(ArrayType);
      BinWriter.I32(w, art.dim);
      Write(w, ss, art.ty)
   |KPointer:
      Write(w, ss, ty(PointerType).ty)
   |KRecord:
      rty := ty(RecordType);
      IF BinWriter.Cond(w, rty.base # NIL) THEN
         Write(w, ss, rty.base)
      END;
      rp := rty.fields;
      (* Since we're exluding some fields, we have to write out
         the bool sentinels normally handled by BinWriter.Cond *)
      WHILE rp # NIL DO
         IF ~(Synthetic IN rp.flags) THEN
            BinWriter.Bool(w, TRUE);
            BinWriter.String(w, rp.name);
            Write(w, ss, rp.ty);
            BinWriter.I32(w, SYSTEM.VAL(INTEGER, rp.flags));
         END;
         rp := rp.next
      END;
      BinWriter.Bool(w, FALSE);
   |KProcedure:
      pty := ty(ProcType);
      IF BinWriter.Cond(w, pty.returnTy # NIL) THEN
         Write(w, ss, pty.returnTy)
      END;
      pp := pty.params;
      WHILE BinWriter.Cond(w, pp # NIL) DO
         BinWriter.String(w, pp.name);
         BinWriter.I32(w, pp.seekpos);
         Write(w, ss, pp.ty);
         pp := pp.next
      END
   END;
END Write;

PROCEDURE ReadImpl(VAR r: BinReader.T; VAR ss: SerialState): Type;
VAR kind, vers, flags: INTEGER;
    art: ArrayType;
    rty: RecordType;
    rp: RecordField;
    pty: ProcType;
    point: PointerType;
    pp: ProcParam;
    rv: Type;
    sst: SSType;
    qn: POINTER TO QualName;
    t0: INTEGER;
BEGIN
   BinReader.I32(r, vers); ASSERT(vers = BinFmtVer);
   BinReader.I8(r, kind);
   BinReader.I32(r, flags);
   IF BinReader.Cond(r) THEN
      NEW(qn);
      BinReader.String(r, qn.module);
      BinReader.String(r, qn.name)
   ELSE
      qn := NIL
   END;
   CASE kind OF
   KByte..KSet, KVoid, KAny,KPrimName:
      rv := MkPrim(kind);
   |KNamedRef:
      sst := RememberSerType(ss, qn^);
      IF sst # NIL THEN
         rv := sst.ty
      ELSE
         rv := MkPrim(KNamedRef);
         rv.srcName := qn;
      END
   |KArray:
      art := MkArrayType();
      BinReader.I32(r, art.dim);
      art.ty := ReadImpl(r, ss);
      rv := art
   |KPointer:
      point := MkPointerType();
      point.ty :=  ReadImpl(r, ss);
      rv := point
   |KRecord:
      rty := MkRecordType();
      IF BinReader.Cond(r) THEN
         rty.base := ReadImpl(r, ss)
      END;
      WHILE BinReader.Cond(r) DO
         rp := MkRecordField();
         BinReader.String(r, rp.name);
         rp.ty := ReadImpl(r, ss);
         BinReader.I32(r, t0);
         rp.flags := SYSTEM.VAL(SET, t0);
         rp.next := rty.fields;
         rty.fields := rp
      END;
      RevRecordFieldList(rty.fields);
      rv := rty
   |KProcedure:
      pty := MkProcType();
      IF BinReader.Cond(r) THEN
         pty.returnTy := ReadImpl(r, ss)
      END;
      WHILE BinReader.Cond(r) DO
         pp := MkProcParam();
         BinReader.String(r, pp.name);
         BinReader.I32(r, pp.seekpos);
         pp.ty := ReadImpl(r, ss);
         pp.next := pty.params;
         pty.params := pp
      END;
      RevProcParam(pty.params);
      rv := pty
   END;
   rv.flags := SYSTEM.VAL(SET, flags);
   IF kind # KNamedRef THEN
      rv.srcName := qn;
      NoteSerType(ss, rv)
   END;
   RETURN rv
END ReadImpl;

PROCEDURE FixupFwdRefs(ss: SerialState; ty: Type): Type;
   (* Recursion is broken while writing with the NamedRef
      entries.  This function re-connects the breaks
      after a read. *)
VAR rv: Type;
    sst: SSType;
    art: ArrayType;
    pot: PointerType;
    rty: RecordType;
    rp: RecordField;
    procty: ProcType;
    pp: ProcParam;
BEGIN
   rv := ty;
   IF ~(Visited IN ty.flags) THEN
      INCL(ty.flags, Visited);
      CASE ty.kind OF
      KArray:
         art := ty(ArrayType);
         art.ty := FixupFwdRefs(ss, art.ty)
      |KPointer:
         pot := ty(PointerType);
         pot.ty := FixupFwdRefs(ss, pot.ty)
      |KRecord:
         rty := ty(RecordType);
         IF rty.base # NIL THEN
            rty.base := FixupFwdRefs(ss, rty.base)
         END;
         rp := rty.fields;
         WHILE rp # NIL DO
            rp.ty := FixupFwdRefs(ss, rp.ty);
            rp := rp.next
         END
      |KProcedure:
         procty := ty(ProcType); 
         IF procty.returnTy # NIL THEN
            procty.returnTy := FixupFwdRefs(ss, procty.returnTy)
         END;
         pp := procty.params;
         WHILE pp # NIL DO
            pp.ty := FixupFwdRefs(ss, pp.ty);
            pp := pp.next
         END
      |KByte..KSet,KVoid,KAny,KPrimName:
      |KNamedRef:
         sst := RememberSerType(ss,  ty.srcName^);
         IF sst # NIL THEN
            rv := sst.ty
         ELSE
            Dbg.S("Could not resolve type after read: ");
            Dbg.S(ty.srcName.module); Dbg.S("."); Dbg.S(ty.srcName.name);
            Dbg.Ln;
            ASSERT(FALSE)
         END
      END;
   END;
   RETURN rv
END FixupFwdRefs;

PROCEDURE Read*(VAR r: BinReader.T; VAR ss: SerialState): Type;
   (* Reads a type previously saved with Write() from a file *)
VAR rv: Type;
BEGIN
   rv := ReadImpl(r, ss);
   RETURN FixupFwdRefs(ss, rv)
END Read;

PROCEDURE AcceptPrimTyName*(t: Ast.T; scan: Lex.T): Type;
   (* If t is qualident containing a primitive type name, 
      it returns that type.  Returns NIL otherwise. *)
VAR rv: Type;
    br: Ast.Branch;
    cld: Ast.T;
    term: Ast.Terminal;
BEGIN
   rv := NIL;
   IF t IS Ast.Branch THEN
      br := t(Ast.Branch);
      IF br.kind = Ast.BkQualIdent THEN
         cld := Ast.GetChild(br, 0);
         IF cld = NIL THEN
            cld := Ast.GetChild(br, 1);
            IF cld IS Ast.Terminal THEN
               term := cld(Ast.Terminal);
               rv := LookupPrimitiveType(scan, term.tok);
               IF rv.kind = KTypeError THEN rv := NIL END
            END
         END
      END
   END
   RETURN rv
END AcceptPrimTyName;


(* Given a type constant for a primitive type, 
   returns an instance for it.  Returns NIL if
   the type requested is not a primitive, but a 
   structured type like an array record or pointer. *)
PROCEDURE PrimitiveType*(kind: INTEGER): Type;
   RETURN PrimTyInstances[kind]
END PrimitiveType;

PROCEDURE SetupTables();
VAR arty: ArrayType;
    pty: PointerType;
   PROCEDURE PAssoc(n: ARRAY OF CHAR; k: INTEGER);
   BEGIN
      PrimNames[k] := n;
      PrimTyKinds[k] := k;
      PrimTyInstances[k] := MkPrim(k)
   END PAssoc;
BEGIN
   PAssoc("BYTE", KByte);
   PAssoc("INTEGER", KInteger);
   PAssoc("BOOLEAN", KBoolean);
   PAssoc("REAL", KReal);
   PAssoc("CHAR", KChar);
   PAssoc("SET", KSet);
   PAssoc("ANY", KAny);
   PAssoc("PRIMTYNAME",  KPrimName);

   VoidType := MkPrim(KVoid);
   ErrorType := MkPrim(KTypeError);
   arty := MkArrayType();
   arty.dim := 2;
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
