(* Symbol table.  Largely a placeholder at the moment *)
MODULE Symtab;
IMPORT
   Ast, Cvt:=extConvert, Dbg, Lex:=Scanner, Strings, Ty:=Types;

CONST
   MaxNameLen=64;

   (* Primitive type kinds we can evaluate to *)
   KByte*=Ty.KByte; KInteger*=Ty.KInteger; KBoolean*=Ty.KBoolean; 
   KReal*=Ty.KReal; KChar*=Ty.KChar; KSet*=Ty.KSet;
   KError*=-1;

TYPE   
   (* Possible values for constants *)
   ConstVal* = RECORD
      kind*: INTEGER;  (* The K* constants, or KError *)
      ival*: INTEGER;
      bval*: BOOLEAN;
      rval*: REAL;
      cval*: CHAR;
      setval*: SET;
      byval*: BYTE
   END;

   EvalError* = POINTER TO EvalErrDesc;
   EvalErrDesc* = RECORD
      msg*: ARRAY 128 OF CHAR;
      pos*: Ast.SourcePos
   END;

   Constant* = POINTER TO ConstantDesc;
   ConstantDesc* = RECORD
      name*: ARRAY MaxNameLen OF CHAR;
      exported*: BOOLEAN;
      val*: ConstVal;
      next: Constant
   END;

   (* Frame. Multiple variables in a frame, for a proc or 
      a module *)

   (* Association of name and type for frame variables for 
      modules and procedures. This can also be used to keep
      up with named types. *)
   FrameVar* = POINTER TO FrameVarDesc;
   FrameVarDesc* = RECORD
      name*: ARRAY MaxNameLen OF CHAR;
      ty*: Ty.Type;
      export*: BOOLEAN;
      next*: FrameVar
   END;


   (* Record of named items in a module.  It's expected
      that this will contain all items for the module 
      being compiled, and just the exported items for 
      imported modules *)
   Module* = POINTER TO ModuleDesc;
   ModuleDesc* = RECORD
      name*: ARRAY MaxNameLen OF CHAR;
      imports: Module;
         (* Linked list of imported modules *)

      importNext: Module;
         (* link for list of module imports *)

      types*: FrameVar;
         (* Not really vars, but named types *)

      vars*: FrameVar;
         (* Link for list of module vars *)
      
      constants*: Constant;

      errs*: ARRAY 16 OF EvalErrDesc;
      nofErrs*: INTEGER
         (* Errors recording while loading the module, or doing
            semantic checking *)

   END; 

VAR
   (* fwd decl *)
   CvtStrucType: PROCEDURE(mod: Module; t: Ast.T; scan: Lex.T; VAR rv: Ty.Type): EvalError;

PROCEDURE MkModule(): Module;
VAR rv: Module;
BEGIN
   NEW(rv);
   rv.imports := NIL;
   rv.importNext := NIL;
   rv.constants := NIL;
   rv.types := NIL;
   rv.vars := NIL;
   rv.nofErrs := 0;
   RETURN rv
END MkModule;


PROCEDURE StringEq(a, b: ARRAY OF CHAR): BOOLEAN;
VAR rv: BOOLEAN;
    i, alen: INTEGER;
BEGIN
   rv := TRUE;
   i := 0;
   alen := Strings.Length(a);
   WHILE rv & (i <= alen) DO  (* Include 0X, not prefix matching here *)
      IF a[i] # b[i] THEN rv := FALSE
      ELSE INC(i) END
   END;
   RETURN rv
END StringEq;


PROCEDURE LookupImport*(cur: Module; name: ARRAY OF CHAR): Module;
VAR rv, x: Module;
BEGIN
   x := cur.imports;
   rv := NIL;
   WHILE (rv = NIL) & (x # NIL) DO
      IF StringEq(name, x.name) THEN
         rv := x
      ELSE
         x := x.importNext
      END
   END
   RETURN rv
END LookupImport;

PROCEDURE FindConst*(m: Module; n: Ty.QualName): Constant;
VAR c: Constant;
    done: BOOLEAN;
BEGIN
   IF Ty.IsQualified(n) THEN
      m := LookupImport(m, n.module)
   END;
   IF m # NIL THEN
      c := m.constants;
      done := FALSE;
      WHILE ~done & (c # NIL) DO
         IF StringEq(c.name, n.name) THEN
            done := TRUE
         ELSE
            c := c.next
         END
      END
   ELSE
      c := NIL
   END;
   RETURN c
END FindConst;

PROCEDURE LookupConst(m: Module; n: Ty.QualName; VAR dest: ConstVal): BOOLEAN;
VAR rv: BOOLEAN;
    c: Constant;
BEGIN
   c := FindConst(m, n);
   IF c = NIL THEN
      rv := FALSE
   ELSE
      dest := c.val;
      rv := TRUE
   END;
   RETURN rv
END LookupConst;

PROCEDURE FindType*(m: Module; n: Ty.QualName): FrameVar; 
VAR done: BOOLEAN;
    rv: FrameVar;
BEGIN
   IF Ty.IsQualified(n) THEN
      m := LookupImport(m, n.module)
   END;
   IF m # NIL THEN
      rv := m.types;
      done := FALSE;
      WHILE ~done & (rv # NIL) DO
         IF StringEq(rv.name, n.name) THEN
            done := TRUE
         ELSE
            rv := rv.next
         END
      END
   ELSE
      rv := NIL
   END;
   RETURN rv
END FindType;

(* Asserts that a given child of a branch is a terminal, and returns it *)
PROCEDURE TermAt(br: Ast.Branch; i: INTEGER): Ast.Terminal;
VAR x: Ast.T;
BEGIN
   x := Ast.GetChild(br, i);
   RETURN x(Ast.Terminal)
END TermAt;

PROCEDURE BranchAt(br: Ast.Branch; i: INTEGER): Ast.Branch;
VAR x: Ast.T;
    rv: Ast.Branch;
BEGIN
   x := Ast.GetChild(br, i);
   IF x = NIL THEN
      rv := NIL
   ELSE
      rv := x(Ast.Branch)
   END
   RETURN rv
END BranchAt;

PROCEDURE MkEvalError(msg: ARRAY OF CHAR; scan: Lex.T; 
                      ast: Ast.T): EvalError;
VAR rv: EvalError;
    t: Ast.Terminal;
BEGIN
   NEW(rv);
   rv.msg := msg;
   IF ast IS Ast.Terminal THEN 
      t := ast(Ast.Terminal);
      rv.pos.line := Lex.LineForPos(scan, t.tok.start);
      rv.pos.col := Lex.ColForPos(scan, t.tok.start);
      rv.pos.seek := t.tok.start
   ELSE 
      Ast.Position(ast, scan, rv.pos)
   END
   RETURN rv
END MkEvalError;

(* Writes the error out to the console *)
PROCEDURE Announce*(ee: EvalErrDesc; file: ARRAY OF CHAR);
BEGIN
   Dbg.S(file); 
   Dbg.S("(");Dbg.I(ee.pos.line);Dbg.S(",");Dbg.I(ee.pos.col);Dbg.S(")");
   Dbg.S(": error: "); Dbg.S(ee.msg);
   Dbg.Ln
END Announce;

PROCEDURE EvalTerminal(cur: Module; scan: Lex.T; t: Ast.Terminal; 
                       VAR dest: ConstVal): EvalError;
VAR rv: EvalError;
    buf: ARRAY 64 OF CHAR;
BEGIN
   rv := NIL;
   IF (t.tok.kind = Lex.ConstInt) OR (t.tok.kind = Lex.ConstHex) THEN
      Lex.Extract(scan, t.tok, buf);
      Cvt.StringToInt(buf, dest.ival, dest.bval);
      IF dest.bval THEN
         dest.kind := KInteger
      ELSE
         rv := MkEvalError("Could not parse int literal", scan, t)
      END
   ELSE
      rv := MkEvalError("Unrecognized constant value", scan, t)
   END

   RETURN rv
END EvalTerminal;

PROCEDURE Negate(VAR dest: ConstVal): BOOLEAN;
VAR rv: BOOLEAN;
BEGIN
   rv := TRUE;
   IF dest.kind = KInteger THEN
      dest.ival := -dest.ival
   ELSIF dest.kind = KReal THEN
      dest.rval := -dest.rval
   ELSE
      rv := FALSE
   END;
   RETURN rv
END Negate;

PROCEDURE EvalConstExpr*(cur: Module; scan: Lex.T; expr: Ast.T; 
                         VAR dest: ConstVal): EvalError;
VAR rv: EvalError;
    t, op: Ast.Terminal;
    any: Ast.T;
    br: Ast.Branch;
    lhs, rhs: ConstVal;
    qname: Ty.QualName;
BEGIN
   rv := NIL;
   IF expr IS Ast.Terminal THEN
      rv := EvalTerminal(cur, scan, expr(Ast.Terminal), dest)
   ELSIF expr IS Ast.Branch THEN
      br := expr(Ast.Branch);
      IF br.kind = Ast.BkUnOp THEN
         rv := EvalConstExpr(cur, scan, Ast.GetChild(br, 1), dest);
         t := TermAt(br, 0);
         IF (rv = NIL) & (t.tok.kind = Lex.MINUS) THEN
            IF ~Negate(dest) THEN
               rv := MkEvalError("Bad type for negation", scan, t)
            END
         END
      ELSIF br.kind = Ast.BkBinOp THEN
         op := TermAt(br, 1);
         rv := EvalConstExpr(cur, scan, Ast.GetChild(br, 0), lhs);
         IF rv = NIL THEN
            rv := EvalConstExpr(cur, scan, Ast.GetChild(br, 2), rhs);
            IF rv = NIL THEN
               IF lhs.kind # rhs.kind THEN
                  rv := MkEvalError("Mismatched binary operand types.", scan, 
                                    op)
               ELSE
                  IF op.tok.kind = Lex.PLUS THEN
                     dest := lhs;
                     IF lhs.kind = KInteger THEN
                        dest.ival := dest.ival + rhs.ival
                     ELSIF lhs.kind = KReal THEN
                        dest.rval := dest.rval + rhs.rval
                     ELSIF lhs.kind = KSet THEN
                        dest.setval := dest.setval + rhs.setval;
                     ELSIF lhs.kind = KByte THEN
                        dest.byval := dest.byval + rhs.byval
                     ELSE
                        rv := MkEvalError("+ not defined for types", scan, op)
                     END
                  ELSIF op.tok.kind = Lex.MINUS THEN
                     dest := lhs;
                     IF lhs.kind = KInteger THEN
                        dest.ival := dest.ival - rhs.ival
                     ELSIF lhs.kind = KReal THEN
                        dest.rval := dest.rval - rhs.rval
                     ELSIF lhs.kind = KSet THEN
                        dest.setval := dest.setval - rhs.setval;
                     ELSIF lhs.kind = KByte THEN
                        dest.byval := dest.byval - rhs.byval
                     ELSE
                        rv := MkEvalError("- not defined for types", scan, op)
                     END     
                  ELSIF op.tok.kind = Lex.ASTERISK THEN
                     dest := lhs;
                     IF lhs.kind = KInteger THEN
                        dest.ival := dest.ival * rhs.ival
                     ELSIF lhs.kind = KReal THEN
                        dest.rval := dest.rval * rhs.rval
                     ELSIF lhs.kind = KSet THEN
                        dest.setval := dest.setval * rhs.setval;
                     ELSIF lhs.kind = KByte THEN
                        dest.byval := dest.byval * rhs.byval
                     ELSE
                        rv := MkEvalError("* not defined for types", scan, op)
                     END     
                  ELSE
                     rv := MkEvalError("Unrecognized binary op", scan, op)
                  END
               END
            END
         END
      ELSIF br.kind = Ast.BkQualIdent THEN
         Ty.GetQualName(br, scan, qname);
         IF ~LookupConst(cur, qname, dest) THEN
            any := Ast.GetChild(br, 1);
            rv := MkEvalError("Unknown constant", scan, any(Ast.Terminal))
         END
      ELSE
         rv := MkEvalError("Unrecognized constant value", scan, br)
      END
   ELSE
      ASSERT(FALSE)
   END
   RETURN rv
END EvalConstExpr;

PROCEDURE TyCvtQualIdent(mod: Module; br: Ast.Branch; scan: Lex.T; 
                         VAR rv: Ty.Type): EvalError;
VAR err: EvalError;
    qn: Ty.QualName;
    fv: FrameVar;
    tk: INTEGER;
    term: Ast.Terminal;
BEGIN
   err := NIL;
   Ty.GetQualName(br, scan, qn);
   fv := FindType(mod, qn);
   IF fv # NIL THEN
      rv := fv.ty
   ELSIF ~Ty.IsQualified(qn) THEN
      (* Could be primitive *)
      term := TermAt(br, 1);
      tk := Ty.LookupPrimitive(scan, term.tok);
      IF tk # Ty.KTypeError THEN
         rv := Ty.MkPrim(tk)
      ELSE
         err := MkEvalError("Could not find type", scan, br)
      END
   ELSE
      err := MkEvalError("Could not find type in imported module", scan, br)
   END
   RETURN err
END TyCvtQualIdent;

PROCEDURE TyCvtArrayType(mod: Module; br: Ast.Branch; scan: Lex.T; 
                         VAR rv: Ty.Type): EvalError;
VAR err: EvalError;
    art: Ty.ArrayType;
    cv: ConstVal;
    dms: Ast.Branch;
    x: Ast.T;
    i: INTEGER;
BEGIN
   art := Ty.MkArrayType();
   dms := BranchAt(br, Ast.ArrayTypeDims);
   art.ndims := dms.childLen;
   FOR i := 0 TO art.ndims-1 DO
      x := Ast.GetChild(dms, i);
      err := EvalConstExpr(mod, scan, x, cv);
      IF err = NIL THEN
         IF cv.kind = KInteger THEN
            art.dims[i] := cv.ival
         ELSE
            err := MkEvalError("Array dim not integer", scan, x)
         END
      END
   END;
   IF err = NIL THEN
      err := CvtStrucType(mod, Ast.GetChild(br, Ast.ArrayTypeType),
                          scan, art.ty);
      IF err = NIL THEN
         rv := art
      END
   END

   RETURN err
END TyCvtArrayType;

PROCEDURE TyCvtPointer(mod: Module; br: Ast.Branch; scan: Lex.T; 
                       VAR rv: Ty.Type): EvalError;
VAR err: EvalError;
    pt: Ty.PointerType;
    dt: Ty.DeferredTarget;
    qn: Ty.QualName;
BEGIN
   pt := Ty.MkPointerType();
   err := CvtStrucType(mod, Ast.GetChild(br, 0), scan, pt.ty);
   IF err # NIL THEN
         Ty.GetQualName(BranchAt(br, 0), scan, qn);
         IF ~Ty.IsQualified(qn) THEN
            (* If this is not a reference to a type in another module, 
               but another type in this section, defer resolution of the
               target type until entire type section has been processed *)
            dt := Ty.MkDeferredTarget();
            dt.ast := br;
            Strings.Append(qn.name, dt.name);
            pt.ty := dt;
            rv := pt;
            err := NIL;
         END
   ELSE
      IF pt.ty.kind = Ty.KRecord THEN
         rv := pt
      ELSE
         err := MkEvalError("Pointer only point to records", scan, br)
      END
   END

   RETURN err
END TyCvtPointer;

PROCEDURE TyCvtRecord(mod: Module; br: Ast.Branch; scan: Lex.T; 
                      VAR rv: Ty.Type): EvalError;
VAR err: EvalError;
    rt: Ty.RecordType;
    fld: Ty.RecordField;
    base, flds, flist, lsty, nmlst: Ast.Branch;
    i, j: INTEGER;
    ftype: Ty.Type;
    fname: Ast.Terminal;
BEGIN
    rt := Ty.MkRecordType();
    base := BranchAt(br, Ast.RecordBaseType);
    IF base # NIL THEN
      err := CvtStrucType(mod, base, scan, rt.base)
    END;
    IF err = NIL THEN
      flds := BranchAt(br, Ast.RecordFieldList);
      i := 0;
      WHILE (err = NIL) & (i <= flds.childLen-1) DO
         (* Field list is list of names, and last entry is type *)
         flist := BranchAt(flds, i);
         lsty := BranchAt(flist, Ast.FieldListType);
         err := CvtStrucType(mod, lsty, scan, ftype);
         IF err = NIL THEN
            nmlst := BranchAt(flist, Ast.FieldListIdents);
            FOR j := 0 TO nmlst.childLen-1 DO
               fld := Ty.MkRecordField();
               fname := TermAt(nmlst, j);
               Lex.Extract(scan, fname.tok, fld.name);
               fld.ty := ftype;
               fld.next := rt.fields;
               rt.fields := fld
            END
         END;
         INC(i)
      END
    END;
    IF err = NIL THEN rv := rt END;         

    RETURN err
END TyCvtRecord;

(* Creates a type from an Ast entry.  The entry can either
   be a terminal for a primitive type we recognize, or something
   parsed by Ast.ParseStrucType *)
PROCEDURE CvtStrucTypeImpl(mod: Module; t: Ast.T; scan: Lex.T; VAR rv: Ty.Type): EvalError;
VAR err: EvalError;
    term: Ast.Terminal;
    br: Ast.Branch;
BEGIN
   IF t IS Ast.Terminal THEN
      err := MkEvalError("Unrecognized type", scan, term)
   ELSIF t IS Ast.Branch THEN
      br := t(Ast.Branch);
      IF br.kind = Ast.BkQualIdent THEN
         err := TyCvtQualIdent(mod, br, scan, rv)
      ELSIF br.kind = Ast.BkArrayType THEN
         err := TyCvtArrayType(mod, br, scan, rv)
      ELSIF br.kind = Ast.BkPointer THEN
         err := TyCvtPointer(mod, br, scan, rv)
      ELSIF br.kind = Ast.BkRecordType THEN
         err := TyCvtRecord(mod, br, scan, rv)
      ELSE
         err := MkEvalError("Unrecognized type", scan, br) 
      END
   END
   RETURN err
END CvtStrucTypeImpl;

PROCEDURE LoadImports(imps: Ast.Branch; rv: Module; scan: Lex.T);
BEGIN
   IF imps # NIL THEN
      ASSERT(imps.kind = Ast.BkImportList);
      (*TODO: in the future this will load up the the module descs 
        that get saved when a module is "compiled" *)   
   END
END LoadImports;

PROCEDURE MkConstant(): Constant;
VAR rv: Constant;
BEGIN
   NEW(rv);
   rv.exported := FALSE;
   rv.next := NIL;
   rv.val.kind := KError;
   RETURN rv
END MkConstant; 

PROCEDURE AddErr(mod: Module; err: EvalError); 
BEGIN
   IF mod.nofErrs < LEN(mod.errs) THEN
      mod.errs[mod.nofErrs] := err^;
      INC(mod.nofErrs)
   END
END AddErr;

PROCEDURE LoadVars(consts: Ast.Branch; rv: Module; scan: Lex.T);
VAR i: INTEGER;
BEGIN
   (* TODO: need the function to parse types in the Types module *)
END LoadVars;

PROCEDURE LoadConsts(consts: Ast.Branch; rv: Module; scan: Lex.T);
VAR i: INTEGER;
    cd: Constant;
    br: Ast.Branch;
    id: Ast.Terminal;
    err: EvalError;
BEGIN
   IF consts # NIL THEN
      FOR i := 0 TO consts.childLen-1 DO
         cd := MkConstant();
         br := BranchAt(consts, i);
         ASSERT(br.kind = Ast.BkConstDeclaration);
         id := TermAt(br, 0);
         Lex.Extract(scan, id.tok, cd.name);
         cd.exported := id.export;
         err := EvalConstExpr(rv, scan, Ast.GetChild(br, 1), cd.val);
         IF err # NIL THEN
            AddErr(rv, err)
         ELSE
            cd.next := rv.constants;
            rv.constants := cd
         END
      END
   END
END LoadConsts;

PROCEDURE FixupDeferrals(mod: Module; scan: Lex.T); 
VAR fv, target: FrameVar;
    pt: Ty.PointerType;
    rt: Ty.Type;
    err: EvalError;
    qname: Ty.QualName;
BEGIN
   fv := mod.types;
   WHILE fv # NIL DO
      IF fv.ty IS Ty.PointerType THEN
         pt := fv.ty(Ty.PointerType);
         IF pt.ty IS Ty.DeferredTarget THEN
            qname.module := "";
            qname.name := "";
            Strings.Append(pt.ty(Ty.DeferredTarget).name, qname.name);
            target := FindType(mod, qname);
            IF target = NIL THEN
               err := MkEvalError("Can not resolve pointer target to type",
                                  scan, pt.ty(Ty.DeferredTarget).ast);
               AddErr(mod, err)
            ELSE
               pt.ty := target.ty
            END
         END
      END;
      fv := fv.next
   END
END FixupDeferrals;

PROCEDURE LoadTypes(types: Ast.Branch; mod: Module; scan: Lex.T);
VAR br: Ast.Branch;
    i: INTEGER;
    ty: FrameVar;
    t: Ast.Terminal;
    err: EvalError;
BEGIN
   IF types # NIL THEN
      ASSERT(types.kind = Ast.BkTypeDeclSeq);
      FOR i := 0 TO types.childLen-1 DO
         br := BranchAt(types, i);
         ASSERT(br.kind = Ast.BkTypeDeclaration);
         NEW(ty);
         err := CvtStrucType(mod, Ast.GetChild(br, Ast.TypeDeclVal), scan,
                                               ty.ty);
         IF err = NIL THEN
            t := TermAt(br, Ast.TypeDeclName);
            Lex.Extract(scan, t.tok, ty.name);
            ty.export := t.export;
            ty.next := mod.types;
            mod.types := ty
         ELSE
            AddErr(mod, err)
         END
      END
   END
END LoadTypes;

PROCEDURE LoadDecls(decls: Ast.Branch; rv: Module; scan: Lex.T);
BEGIN
   IF decls # NIL THEN
      ASSERT(decls.kind = Ast.BkDeclarationSequence);
      LoadConsts(BranchAt(decls, Ast.DeclSeqConsts), rv, scan); 
      LoadTypes(BranchAt(decls, Ast.DeclSeqTypes), rv, scan);
      FixupDeferrals(rv, scan);
      LoadVars(BranchAt(decls, Ast.DeclSeqVars), rv, scan)
   END
END LoadDecls;

(* Create a Module for the AST version of a Module.   Populates the 
   errs/nofErrs fields of the module if there's errors *)
PROCEDURE BuildModule*(ast: Ast.T; scan: Lex.T): Module;
VAR rv: Module;
    mod: Ast.Branch;
    term: Ast.Terminal;
BEGIN
   rv := MkModule();
   mod := ast(Ast.Branch);
   ASSERT(mod.kind = Ast.BkModule);
   term := TermAt(mod, Ast.ModuleName);
   Lex.Extract(scan, term.tok, rv.name);
   LoadImports(BranchAt(mod, Ast.ModuleImports), rv, scan);
   IF rv.nofErrs = 0 THEN
      LoadDecls(BranchAt(mod, Ast.ModuleDecls), rv, scan)
   END;

   RETURN rv   
END BuildModule;

BEGIN
   CvtStrucType := CvtStrucTypeImpl
END Symtab.
