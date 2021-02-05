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

   (* Frame flags *)
   ffModule*=0;                  (* module level frame *)
   ffProcParams*=1;              (* Separate frame just for proc params *)


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

   (* Association of name and type. Used for types and vars. *)
   TypeSym* = POINTER TO TypeSymDesc;
   Frame* = POINTER TO FrameDesc;
   TypeSymDesc* = RECORD
      name*: ARRAY MaxNameLen OF CHAR;
      ty*: Ty.Type;
      export*: BOOLEAN;
      (* If this name has a scope, this is the frame for the scope *)
      frame*: Frame;
      next*: TypeSym
   END;

   (* Iterator that takes care of details of iterating
      through more than one typesym list *)
   TSIter* = POINTER TO TSIterDesc;
   TSIterDesc* = RECORD
      curframe: Frame;
      cur: TypeSym;
      op*: RECORD
         Next*: PROCEDURE(it: TSIter): TypeSym
      END
   END;

   (* A frame of variables and constants, types and procedures.  
      They can be linked and searched.  There will be a frame for
      a module (TODO) and procedures will have them as well.  *)
   FrameDesc* = RECORD
      (* ff* flags *)
      flags*: SET; 
      types*: TypeSym;
      constants*: Constant;
      vars*: TypeSym;
      procedures*: TypeSym;
      (* The next enclosing scope/frame *)
      searchNext*: Frame;
      (* used for the allFrames linked list in Modules *)
      next: Frame
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

      frame*: Frame;
         (* Search scope for the module's declarations *)

      (* Linked list along the Frame.next member.  For convenience
         in iterating all declarations, as the frame's nextSearch
         links form a tree that's more complicated to traverse *)
      allFrames*: Frame;
      errs*: ARRAY 16 OF EvalErrDesc;
      nofErrs*: INTEGER
         (* Errors recording while loading the module, or doing
            semantic checking *)
   END; 

VAR
   (* fwd decl *)
   CvtStrucType: PROCEDURE(mod: Module; frame: Frame; t: Ast.T; scan: Lex.T; 
                           VAR rv: Ty.Type): EvalError;
   LoadDecls: PROCEDURE(procDecls: Ast.Branch; rv: Module; frame: Frame; scan: Lex.T);


PROCEDURE MkFrame(owner: Module; chainTo: Frame): Frame;
VAR rv: Frame;
BEGIN
   NEW(rv);
   rv.flags := {};
   rv.types := NIL;
   rv.constants := NIL;
   rv.vars := NIL;
   rv.procedures := NIL;
   rv.searchNext := chainTo;
   rv.next := owner.allFrames;
   owner.allFrames := rv;
   RETURN rv
END MkFrame; 

PROCEDURE MkModule(): Module;
VAR rv: Module;
BEGIN
   NEW(rv);
   rv.allFrames := NIL;
   rv.imports := NIL;
   rv.importNext := NIL;
   rv.frame := MkFrame(rv, NIL);
   rv.frame.flags := {ffModule};
   rv.nofErrs := 0;
   RETURN rv
END MkModule;

PROCEDURE MkTypeSym(): TypeSym;
VAR rv: TypeSym;
BEGIN
   NEW(rv);
   rv.name := "";
   rv.ty := NIL;
   rv.export := FALSE;
   rv.next := NIL;
   rv.frame := NIL;
   RETURN rv
END MkTypeSym; 


PROCEDURE RevTypeSymList(VAR l: TypeSym);
VAR x, next, last: TypeSym;
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
END RevTypeSymList;

PROCEDURE IterAllProcs*(mod: Module) : TSIter;
VAR rv: TSIter;
   PROCEDURE AllProcNext(it: TSIter): TypeSym;
   VAR rv: TypeSym;
   BEGIN
      IF it.cur = NIL THEN
         WHILE (it.cur = NIL) & (it.curframe # NIL) DO
            it.cur := it.curframe.procedures;
            it.curframe := it.curframe.next
         END;
      END;
      rv := it.cur;
      IF it.cur # NIL THEN it.cur := it.cur.next END;
      RETURN rv
   END AllProcNext;
BEGIN
   NEW(rv);
   rv.curframe := mod.allFrames;
   rv.cur := NIL;
   rv.op.Next := AllProcNext;
   RETURN rv
END IterAllProcs;

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


PROCEDURE FindImport*(cur: Module; name: ARRAY OF CHAR): Module;
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
END FindImport;

PROCEDURE FindConst*(m: Module; frame: Frame; n: Ty.QualName): Constant;
VAR c: Constant;
    done: BOOLEAN;
BEGIN
   IF Ty.IsQualified(n) THEN
      m := FindImport(m, n.module);
      IF m # NIL THEN frame := m.frame END;
   END;
   IF m # NIL THEN
      done := FALSE;
      WHILE ~done & (frame # NIL) DO
         c := frame.constants;
         WHILE ~done & (c # NIL) DO
            IF StringEq(c.name, n.name) THEN
               done := TRUE
            ELSE
               c := c.next
            END
         END;
         frame := frame.searchNext
      END;
   ELSE
      c := NIL
   END;
   RETURN c
END FindConst;

PROCEDURE LookupConst(m: Module; frame: Frame; n: Ty.QualName; VAR dest: ConstVal): BOOLEAN;
VAR rv: BOOLEAN;
    c: Constant;
BEGIN
   c := FindConst(m, frame, n);
   IF c = NIL THEN
      rv := FALSE
   ELSE
      dest := c.val;
      rv := TRUE
   END;
   RETURN rv
END LookupConst;

PROCEDURE FindTypeSym*(rv: TypeSym; name: ARRAY OF CHAR): TypeSym; 
VAR done: BOOLEAN;
BEGIN
   done := FALSE;
   WHILE ~done & (rv # NIL) DO
      IF StringEq(rv.name, name) THEN
         done := TRUE
      ELSE
         rv := rv.next
      END
   END
   RETURN rv
END FindTypeSym;

PROCEDURE FindProc*(m: Module; frame: Frame; n: Ty.QualName): TypeSym; 
VAR rv: TypeSym;
BEGIN
   IF Ty.IsQualified(n) THEN
      m := FindImport(m, n.module);
      IF m # NIL THEN frame := m.frame END
   END;
   rv := NIL;
   WHILE (frame # NIL) & (rv = NIL) DO
      rv := FindTypeSym(frame.procedures, n.name);
      frame := frame.searchNext
   END;
   RETURN rv
END FindProc;

PROCEDURE FindType*(m: Module; frame: Frame; n: Ty.QualName): TypeSym; 
VAR rv: TypeSym;
BEGIN
   IF Ty.IsQualified(n) THEN
      m := FindImport(m, n.module);
      IF m # NIL THEN frame := m.frame END
   END;
   rv := NIL;
   WHILE (frame # NIL) & (rv = NIL) DO
      rv := FindTypeSym(frame.types, n.name);
      frame := frame.searchNext
   END;
   RETURN rv
END FindType;

PROCEDURE MkEvalError*(msg: ARRAY OF CHAR; scan: Lex.T; 
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

PROCEDURE EvalConstExpr*(cur: Module; frame: Frame; scan: Lex.T; expr: Ast.T; 
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
         rv := EvalConstExpr(cur, frame, scan, Ast.GetChild(br, 1), dest);
         t := Ast.TermAt(br, 0);
         IF (rv = NIL) & (t.tok.kind = Lex.MINUS) THEN
            IF ~Negate(dest) THEN
               rv := MkEvalError("Bad type for negation", scan, t)
            END
         END
      ELSIF br.kind = Ast.BkBinOp THEN
         op := Ast.TermAt(br, 1);
         rv := EvalConstExpr(cur, frame, scan, Ast.GetChild(br, 0), lhs);
         IF rv = NIL THEN
            rv := EvalConstExpr(cur, frame, scan, Ast.GetChild(br, 2), rhs);
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
         IF ~LookupConst(cur, frame, qname, dest) THEN
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

PROCEDURE TyCvtQualIdent(mod: Module; frame: Frame; br: Ast.Branch; scan: Lex.T; 
                         VAR rv: Ty.Type): EvalError;
VAR err: EvalError;
    qn: Ty.QualName;
    fv: TypeSym;
    tk: INTEGER;
    term: Ast.Terminal;
BEGIN
   err := NIL;
   Ty.GetQualName(br, scan, qn);
   fv := FindType(mod, frame, qn);
   IF fv # NIL THEN
      rv := fv.ty
   ELSIF ~Ty.IsQualified(qn) THEN
      (* Could be primitive *)
      term := Ast.TermAt(br, 1);
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

PROCEDURE TyCvtArrayType(mod: Module; frame: Frame; br: Ast.Branch; scan: Lex.T; 
                         VAR rv: Ty.Type): EvalError;
VAR err: EvalError;
    art: Ty.ArrayType;
    cv: ConstVal;
    dms: Ast.Branch;
    x: Ast.T;
    i: INTEGER;
BEGIN
   art := Ty.MkArrayType();
   dms := Ast.BranchAt(br, Ast.ArrayTypeDims);
   art.ndims := dms.childLen;
   FOR i := 0 TO art.ndims-1 DO
      x := Ast.GetChild(dms, i);
      err := EvalConstExpr(mod, frame, scan, x, cv);
      IF err = NIL THEN
         IF cv.kind = KInteger THEN
            art.dims[i] := cv.ival
         ELSE
            err := MkEvalError("Array dim not integer", scan, x)
         END
      END
   END;
   IF err = NIL THEN
      err := CvtStrucType(mod, frame, Ast.GetChild(br, Ast.ArrayTypeType),
                          scan, art.ty);
      IF err = NIL THEN
         rv := art
      END
   END

   RETURN err
END TyCvtArrayType;

(* For a FormalParameters node, creates Ty.ProcParam for all 
   parameters and links them into "paramList". Returns the 
   list in reverse order, caller can reverse it if it cares. *)
PROCEDURE TyCvtFormalParams(mod: Module; frame: Frame; formalParams: Ast.Branch; scan: Lex.T;
                            VAR paramList: Ty.ProcParam): EvalError;
VAR err: EvalError;
    i, j: INTEGER;
    name: Ast.Terminal;
    fpsec, formalType: Ast.Branch;
    paramTy: Ty.Type;
    fld: Ty.ProcParam;
BEGIN
   err := NIL;
   i := 1;
   WHILE (err = NIL) & (i <= (formalParams.childLen-1)) DO
      fpsec := Ast.BranchAt(formalParams, i);
      formalType := Ast.BranchAt(fpsec, fpsec.childLen-1);
      err := CvtStrucType(mod, frame, Ast.BranchAt(formalType, 0), 
                          scan, paramTy);
      IF err = NIL THEN
         IF Ast.NfVar IN fpsec.flags THEN
            INCL(paramTy.flags, Ty.Var)
         END;
         FOR j := 0 TO fpsec.childLen-2 DO
            fld := Ty.MkProcParam();
            name := Ast.TermAt(fpsec, j);
            Lex.Extract(scan, name.tok, fld.name);
            fld.ty := paramTy;
            paramTy.openArrays := formalType.n;
            fld.next := paramList;
            paramList := fld
         END
      END;
      INC(i)
   END;
   RETURN err
END TyCvtFormalParams;


PROCEDURE TyCvtProc(mod: Module; frame: Frame; br: Ast.Branch; scan: Lex.T;
                    VAR rv: Ty.Type): EvalError;
VAR err: EvalError;
    formalParams, rtype: Ast.Branch;
    pt: Ty.ProcType;
BEGIN
   pt := Ty.MkProcType();
   formalParams := Ast.BranchAt(br, 0);
   rtype := Ast.BranchAt(formalParams, Ast.FormalParamsReturn);
   IF rtype # NIL THEN
      err := CvtStrucType(mod, frame, rtype, scan, pt.returnTy)
   END;
   IF err = NIL THEN
      err := TyCvtFormalParams(mod, frame, formalParams, scan, pt.params)
   END;
   IF err = NIL THEN
      Ty.RevProcParam(pt.params);
      rv := pt
   END;
   RETURN err
END TyCvtProc;

(* Creates a lookup frame containing just procedure parameters.
   The frame vars aren't necessarily in parameter order. *)
PROCEDURE FrameForParams(mod: Module; parent: Frame; proc: Ty.ProcType): Frame;
VAR rv: Frame;
    param: Ty.ProcParam;
    ts: TypeSym;
BEGIN
   rv := MkFrame(mod, parent);
   rv.flags := {ffProcParams};
   param := proc.params;
   WHILE param # NIL DO
      ts := MkTypeSym();
      Strings.Append(param.name, ts.name);
      ts.ty := param.ty;
      ts.next := rv.vars;
      rv.vars := ts;
      param := param.next
   END;
   RETURN rv
END FrameForParams;

PROCEDURE TyCvtProcDecl(mod: Module; frame: Frame; procDecl: Ast.Branch; scan: Lex.T;
                        VAR rv: Ty.Type): EvalError;
VAR err: EvalError;
    pty: Ty.ProcType;
    formalParams, bodyDecls, retType: Ast.Branch;
BEGIN
   err := NIL;
   pty := Ty.MkProcType();
   formalParams := Ast.BranchAt(procDecl, Ast.ProcedureDeclParams);
   pty.body := Ast.BranchAt(procDecl, Ast.ProcedureDeclBody);
   retType := Ast.BranchAt(formalParams, Ast.FormalParamsReturn);
   IF retType # NIL THEN
      err := CvtStrucType(mod, frame, retType, scan, pty.returnTy)
   END;
   IF err = NIL THEN
      err := TyCvtFormalParams(mod, frame, formalParams, scan, pty.params);
   END;
   IF err = NIL THEN
      Ty.RevProcParam(pty.params);
      rv := pty;
      bodyDecls := Ast.BranchAt(pty.body, Ast.ProcBodyDecls);
      IF bodyDecls # NIL THEN
         LoadDecls(bodyDecls, mod, frame, scan)
      END
   END;
   RETURN err
END TyCvtProcDecl; 

PROCEDURE TyCvtPointer(mod: Module; frame: Frame; br: Ast.Branch; scan: Lex.T; 
                       VAR rv: Ty.Type): EvalError;
VAR err: EvalError;
    pt: Ty.PointerType;
    dt: Ty.DeferredTarget;
    qn: Ty.QualName;
BEGIN
   pt := Ty.MkPointerType();
   err := CvtStrucType(mod, frame, Ast.GetChild(br, 0), scan, pt.ty);
   IF err # NIL THEN
         Ty.GetQualName(Ast.BranchAt(br, 0), scan, qn);
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

PROCEDURE TyCvtRecord(mod: Module; frame: Frame; br: Ast.Branch; scan: Lex.T; 
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
    base := Ast.BranchAt(br, Ast.RecordBaseType);
    IF base # NIL THEN
      err := CvtStrucType(mod, frame, base, scan, rt.base)
    END;
    IF err = NIL THEN
      flds := Ast.BranchAt(br, Ast.RecordFieldList);
      i := 0;
      WHILE (err = NIL) & (i <= flds.childLen-1) DO
         (* Field list is list of names, and last entry is type *)
         flist := Ast.BranchAt(flds, i);
         lsty := Ast.BranchAt(flist, Ast.FieldListType);
         err := CvtStrucType(mod, frame, lsty, scan, ftype);
         IF err = NIL THEN
            nmlst := Ast.BranchAt(flist, Ast.FieldListIdents);
            FOR j := 0 TO nmlst.childLen-1 DO
               fld := Ty.MkRecordField();
               fname := Ast.TermAt(nmlst, j);
               Lex.Extract(scan, fname.tok, fld.name);
               fld.ty := ftype;
               fld.next := rt.fields;
               fld.export := fname.export;
               rt.fields := fld
            END
         END;
         INC(i)
      END
    END;
    IF err = NIL THEN 
      Ty.RevRecordFieldList(rt.fields);
      rv := rt 
    END;

    RETURN err
END TyCvtRecord;

(* Creates a type from an Ast entry.  The entry can either
   be a terminal for a primitive type we recognize, or something
   parsed by Ast.ParseStrucType *)
PROCEDURE CvtStrucTypeImpl(mod: Module; frame: Frame; t: Ast.T; scan: Lex.T; VAR rv: Ty.Type): EvalError;
VAR err: EvalError;
    term: Ast.Terminal;
    br: Ast.Branch;
BEGIN
   IF t IS Ast.Terminal THEN
      err := MkEvalError("Unrecognized type", scan, term)
   ELSIF t IS Ast.Branch THEN
      br := t(Ast.Branch);
      IF br.kind = Ast.BkQualIdent THEN
         err := TyCvtQualIdent(mod, frame, br, scan, rv)
      ELSIF br.kind = Ast.BkArrayType THEN
         err := TyCvtArrayType(mod, frame, br, scan, rv)
      ELSIF br.kind = Ast.BkPointer THEN
         err := TyCvtPointer(mod, frame, br, scan, rv)
      ELSIF br.kind = Ast.BkRecordType THEN
         err := TyCvtRecord(mod, frame, br, scan, rv)
      ELSIF br.kind = Ast.BkProcedureType THEN
         err := TyCvtProc(mod, frame, br, scan, rv)
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

PROCEDURE AddErr*(mod: Module; err: EvalError); 
BEGIN
   IF mod.nofErrs < LEN(mod.errs) THEN
      mod.errs[mod.nofErrs] := err^;
      INC(mod.nofErrs)
   END
END AddErr;


(* Creates TypeSyms for variables in a VarDeclaration and
   links them into the list pointed to by varList *)
PROCEDURE CvtAndLinkVars(mod: Module; frame: Frame; varDecl: Ast.Branch; 
                         scan: Lex.T; VAR varList: TypeSym): EvalError;
VAR err: EvalError;
    identList, tyast: Ast.Branch;
    vtype: Ty.Type;
    fv: TypeSym; 
    name: Ast.Terminal;
    i: INTEGER;
BEGIN
   identList := Ast.BranchAt(varDecl, Ast.VarDeclIdents);
   tyast := Ast.BranchAt(varDecl, Ast.VarDeclVal);
   err := CvtStrucType(mod, frame, tyast, scan, vtype);
   IF err = NIL THEN
      FOR i := 0 TO identList.childLen-1 DO
         fv := MkTypeSym();
         name := Ast.TermAt(identList, i);
         Lex.Extract(scan, name.tok, fv.name);
         fv.export := name.export;
         fv.ty := vtype;
         fv.next := varList;
         varList := fv
      END
   END;
   RETURN err
END CvtAndLinkVars; 

PROCEDURE LoadVars(varDeclSeq: Ast.Branch; rv: Module; frame: Frame; scan: Lex.T);
VAR i: INTEGER;
    err: EvalError;
BEGIN
   IF varDeclSeq # NIL THEN
      ASSERT(varDeclSeq.kind = Ast.BkVarDeclSeq);
      FOR i := 0 TO varDeclSeq.childLen-1 DO
         err := CvtAndLinkVars(rv, frame, Ast.BranchAt(varDeclSeq, i), scan, frame.vars);
         IF err # NIL THEN
            AddErr(rv, err)
         END
      END;
      RevTypeSymList(frame.vars)
   END
END LoadVars;

(* Adds all constants from a constant declaration list to "frame" *)
PROCEDURE LoadConsts(consts: Ast.Branch; rv: Module; frame: Frame; scan: Lex.T);
VAR i: INTEGER;
    cd: Constant;
    br: Ast.Branch;
    id: Ast.Terminal;
    err: EvalError;
BEGIN
   IF consts # NIL THEN
      FOR i := 0 TO consts.childLen-1 DO
         cd := MkConstant();
         br := Ast.BranchAt(consts, i);
         ASSERT(br.kind = Ast.BkConstDeclaration);
         id := Ast.TermAt(br, 0);
         Lex.Extract(scan, id.tok, cd.name);
         cd.exported := id.export;
         err := EvalConstExpr(rv, frame, scan, Ast.GetChild(br, 1), cd.val);
         IF err # NIL THEN
            AddErr(rv, err)
         ELSE
            cd.next := frame.constants;
            frame.constants := cd
         END
      END
   END
END LoadConsts;

(* Adds all declared procedures in procDecls to "frame" *)
PROCEDURE LoadProcs(procDecls: Ast.Branch; rv: Module; frame: Frame; scan: Lex.T);
VAR i: INTEGER;
    ent: TypeSym;
    err: EvalError;
    fname: Ast.Terminal;
    procDecl: Ast.Branch;
BEGIN
   IF procDecls # NIL THEN
      FOR i := 0 TO procDecls.childLen-1 DO
         procDecl := Ast.BranchAt(procDecls, i);
         ent := MkTypeSym();
         fname := Ast.TermAt(procDecl, Ast.ProcedureDeclName);
         Lex.Extract(scan, fname.tok, ent.name);
         ent.export := fname.export;
         (* proc decls get their own frame for their child declarations *) 
         ent.frame := MkFrame(rv, frame);
         err := TyCvtProcDecl(rv, ent.frame, procDecl, scan, ent.ty);
         IF err = NIL THEN
            (* And we link in a separate frame for just the procedure 
               parameters, for convenience *)
            ent.frame := FrameForParams(rv, ent.frame, ent.ty(Ty.ProcType));
            ent.frame.flags := {ffProcParams};
            ent.next := frame.procedures;
            frame.procedures := ent
         ELSE
            AddErr(rv, err)
         END
      END;
      RevTypeSymList(frame.procedures)
   END
END LoadProcs; 

PROCEDURE FixupDeferrals(mod: Module; frame: Frame; scan: Lex.T); 
VAR fv, target: TypeSym;
    pt: Ty.PointerType;
    err: EvalError;
    qname: Ty.QualName;
BEGIN
   fv := frame.types;
   WHILE fv # NIL DO
      IF fv.ty IS Ty.PointerType THEN
         pt := fv.ty(Ty.PointerType);
         IF pt.ty IS Ty.DeferredTarget THEN
            qname.module := "";
            qname.name := "";
            Strings.Append(pt.ty(Ty.DeferredTarget).name, qname.name);
            target := FindType(mod, frame, qname);
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

(* Loads all of the types from "types" into "frame" *)
PROCEDURE LoadTypes(types: Ast.Branch; mod: Module; frame: Frame; scan: Lex.T);
VAR br: Ast.Branch;
    i: INTEGER;
    ty: TypeSym;
    t: Ast.Terminal;
    err: EvalError;
BEGIN
   IF types # NIL THEN
      ASSERT(types.kind = Ast.BkTypeDeclSeq);
      FOR i := 0 TO types.childLen-1 DO
         br := Ast.BranchAt(types, i);
         ASSERT(br.kind = Ast.BkTypeDeclaration);
         ty := MkTypeSym();
         err := CvtStrucType(mod, frame, 
                             Ast.GetChild(br, Ast.TypeDeclVal), scan,
                                          ty.ty);
         IF err = NIL THEN
            t := Ast.TermAt(br, Ast.TypeDeclName);
            Lex.Extract(scan, t.tok, ty.name);
            ty.export := t.export;
            ty.next := frame.types;
            frame.types := ty
         ELSE
            AddErr(mod, err)
         END
      END;
      RevTypeSymList(frame.types)
   END
END LoadTypes;

PROCEDURE LoadDeclsImpl(decls: Ast.Branch; rv: Module; frame: Frame; scan: Lex.T);
BEGIN
   IF decls # NIL THEN
      ASSERT(decls.kind = Ast.BkDeclarationSequence);
      LoadConsts(Ast.BranchAt(decls, Ast.DeclSeqConsts), rv, frame, scan); 
      LoadTypes(Ast.BranchAt(decls, Ast.DeclSeqTypes), rv, frame, scan);
      FixupDeferrals(rv, frame, scan);
      LoadVars(Ast.BranchAt(decls, Ast.DeclSeqVars), rv, frame, scan);
      LoadProcs(Ast.BranchAt(decls, Ast.DeclSeqProcs), rv, frame, scan)
   END
END LoadDeclsImpl;

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
   term := Ast.TermAt(mod, Ast.ModuleName);
   Lex.Extract(scan, term.tok, rv.name);
   LoadImports(Ast.BranchAt(mod, Ast.ModuleImports), rv, scan);
   IF rv.nofErrs = 0 THEN
      LoadDecls(Ast.BranchAt(mod, Ast.ModuleDecls), rv, rv.frame, scan)
   END;

   RETURN rv   
END BuildModule;

BEGIN
   CvtStrucType := CvtStrucTypeImpl;
   LoadDecls := LoadDeclsImpl;
END Symtab.
