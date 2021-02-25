(* Symbol table.  Largely a placeholder at the moment *)
MODULE Symtab;
IMPORT
   Ast, BinReader, BinWriter, Config, Cvt:=extConvert, Dbg, Lex:=Scanner, 
   Path, Strings, SYSTEM, Ty:=Types;

CONST
   MaxNameLen=64;

   (* Primitive type kinds we can evaluate to *)
   KByte*=Ty.KByte; KInteger*=Ty.KInteger; KBoolean*=Ty.KBoolean; 
   KReal*=Ty.KReal; KChar*=Ty.KChar; KSet*=Ty.KSet;
   KError*=-1;

   (* Frame flags *)
   ffModule*=0;                  (* module level frame *)
   ffProcParams*=1;              (* Separate frame just for proc params *)

   (* TypeSym kinds *)
   tsVar*=0; tsProc*=1; tsType*=2; tsProcParam*=3; tsConst*=4;

   (* Version for symtab files *)
   SymtabVer=50H; FrameMagic=46H;


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
   OptConstVal* = POINTER TO ConstVal;

   (* Association of name and type. Used for types and vars. *)
   TypeSym* = POINTER TO TypeSymDesc;
   Frame* = POINTER TO FrameDesc;
   TypeSymDesc* = RECORD
      kind*: INTEGER;  (* ts* constants *)
      name*: ARRAY MaxNameLen OF CHAR;
      id*: INTEGER;
         (* Unique for this typesym in the source file it was defined in *)
      ty*: Ty.Type;
      export*: BOOLEAN;
      val*: OptConstVal;
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
      constants*: TypeSym;
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
      name*, localAlias*: ARRAY MaxNameLen OF CHAR;
      imports*: Module;
         (* Linked list of imported modules *)

      importNext*: Module;
         (* link for list of module imports *)

      frame*: Frame;
         (* Search scope for the module's declarations *)

      initBlock*: Frame;
         (* Special frame that just has a procedure for the init block
            of the module.  Can be nil if there is no init code.  Added
            as a frame, so this fake function will be seen when iterating
            through the module procs. *)

      (* Linked list along the Frame.next member.  For convenience
         in iterating all declarations, as the frame's nextSearch
         links form a tree that's more complicated to traverse *)
      allFrames*: Frame;
      errs*: ARRAY 16 OF Ast.SrcErrorDesc;
      nofErrs*: INTEGER
         (* Errors recording while loading the module, or doing
            semantic checking *)
   END; 

   (* Annotates AST nodes with the corresponding TypeSym *)
   TSAnnotation* = POINTER TO TSAnnotationDesc;
   TSAnnotationDesc* = RECORD(Ast.AnnotationDesc)
      ts*: TypeSym
   END;

VAR
   (* fwd decl *)
   CvtStrucType: PROCEDURE(mod: Module; frame: Frame; t: Ast.T; scan: Lex.T; 
                           VAR rv: Ty.Type): Ast.SrcError;
   LoadDecls: PROCEDURE(procDecls: Ast.Branch; rv: Module; frame: Frame; scan: Lex.T);
   FileMagic: ARRAY 5 OF CHAR;

   (* Symtab for builtin functions that are globally available. Should
      only have procedures *)
   Builtins: Module;

PROCEDURE Note*(t: Ast.T; ts: TypeSym);
VAR n: TSAnnotation;
BEGIN
   NEW(n);
   n.ts := ts;
   Ast.Note(t, n)
END Note;

PROCEDURE Remember*(t: Ast.T): TSAnnotation;
VAR an: Ast.Annotation;
    rv: TSAnnotation;
BEGIN
   an := t.notes;
   WHILE (an # NIL) & ~(an IS TSAnnotation) DO
      an := an.anext
   END;
   IF an # NIL THEN
      rv := an(TSAnnotation)
   ELSE
      rv := NIL
   END;
   RETURN rv
END Remember;

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
   rv.initBlock := NIL;
   rv.frame := MkFrame(rv, NIL);
   rv.frame.flags := {ffModule};
   rv.nofErrs := 0;
   RETURN rv
END MkModule;

PROCEDURE MkTypeSym(kind: INTEGER): TypeSym;
VAR rv: TypeSym;
BEGIN
   NEW(rv);
   rv.kind := kind;
   rv.name := "";
   rv.ty := NIL;
   rv.export := FALSE;
   rv.next := NIL;
   rv.frame := NIL;
   rv.id := 0;
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


PROCEDURE FindImport*(cur: Module; name: ARRAY OF CHAR): Module;
VAR rv, x: Module;
BEGIN
   x := cur.imports;
   rv := NIL;
   WHILE (rv = NIL) & (x # NIL) DO
      IF Ast.StringEq(name, x.localAlias) THEN
         rv := x
      ELSE
         x := x.importNext
      END
   END
   RETURN rv
END FindImport;

PROCEDURE FindTypeSym*(rv: TypeSym; name: ARRAY OF CHAR): TypeSym; 
VAR done: BOOLEAN;
BEGIN
   done := FALSE;
   WHILE ~done & (rv # NIL) DO
      IF Ast.StringEq(rv.name, name) THEN
         done := TRUE
      ELSE
         rv := rv.next
      END
   END
   RETURN rv
END FindTypeSym;

(* Searches for a named const in just the given frame *)
PROCEDURE FindConstThisFrame*(m: Module; frame: Frame; n: Ty.QualName): TypeSym;
BEGIN
   IF Ty.IsQualified(n) THEN
      m := FindImport(m, n.module);
      IF m # NIL THEN frame := m.frame END;
   END;
   RETURN FindTypeSym(frame.constants, n.name)
END FindConstThisFrame;

(* Searches for the named const in "frame" and all enclosing scopes, if
   n is not qualified. *)
PROCEDURE FindConst*(m: Module; frame: Frame; n: Ty.QualName): TypeSym;
VAR rv: TypeSym;
BEGIN
   REPEAT
      rv := FindConstThisFrame(m, frame, n);
      frame := frame.searchNext;
   UNTIL (rv # NIL) OR (frame = NIL) OR Ty.IsQualified(n);
   RETURN rv
END FindConst;

PROCEDURE LookupConst(m: Module; frame: Frame; n: Ty.QualName; VAR dest: ConstVal): BOOLEAN;
VAR rv: BOOLEAN;
    c: TypeSym;
BEGIN
   c := FindConst(m, frame, n);
   IF (c = NIL) OR (c.val = NIL) THEN
      rv := FALSE
   ELSE
      dest := c.val^;
      rv := TRUE
   END;
   RETURN rv
END LookupConst;

(* If n is not qualified, searches for the proc just in the given frame *)
PROCEDURE FindProcThisFrame*(m: Module; frame: Frame; n: Ty.QualName): TypeSym; 
BEGIN
   IF Ty.IsQualified(n) THEN
      m := FindImport(m, n.module);
      IF m # NIL THEN frame := m.frame END
   END;
   RETURN FindTypeSym(frame.procedures, n.name)
END FindProcThisFrame;

(* If n is not qualified, searches for the proc with the given name
   in the current and enclosing scopes *)
PROCEDURE FindProc*(m: Module; frame: Frame; n: Ty.QualName): TypeSym; 
VAR rv: TypeSym;
BEGIN
   REPEAT
      rv := FindProcThisFrame(m, frame, n);
      frame := frame.searchNext
   UNTIL (rv # NIL) OR (frame = NIL) OR Ty.IsQualified(n);
   RETURN rv
END FindProc;

(* Looks for the named type just in this given frame *)
PROCEDURE FindTypeThisFrame*(m: Module; frame: Frame; n: Ty.QualName): TypeSym; 
BEGIN
   IF Ty.IsQualified(n) THEN
      m := FindImport(m, n.module);
      IF m # NIL THEN frame := m.frame END
   END;
   RETURN FindTypeSym(frame.types, n.name)
END FindTypeThisFrame;

(* Looks for a type with the given name in all of the enclosing
   scopes, starting at "frame" *)
PROCEDURE FindType*(m: Module; frame: Frame; n: Ty.QualName): TypeSym; 
VAR rv: TypeSym;
BEGIN
   REPEAT
      rv := FindTypeThisFrame(m, frame, n);
      frame := frame.searchNext;
   UNTIL (rv # NIL) OR (frame = NIL) OR Ty.IsQualified(n);
   RETURN rv
END FindType;


(* Find a var, and if found, also returns the frame it 
   was found in.  Looks only in the given frame. *)
PROCEDURE FindVarLocThisFrame*(m: Module; frame: Frame; n: Ty.QualName; 
                               VAR foundIn: Frame): TypeSym; 
VAR rv: TypeSym;
BEGIN
   IF Ty.IsQualified(n) THEN
      m := FindImport(m, n.module);
      IF m # NIL THEN frame := m.frame END
   END;
   rv := NIL;
   rv := FindTypeSym(frame.vars, n.name);
   IF rv # NIL THEN foundIn := frame END;
   RETURN rv
END FindVarLocThisFrame;

PROCEDURE FindVarLoc*(m: Module; frame: Frame; n: Ty.QualName; 
                      VAR foundIn: Frame): TypeSym; 
VAR rv: TypeSym;
BEGIN
   REPEAT
      rv := FindVarLocThisFrame(m, frame, n, foundIn);
      frame := frame.searchNext
   UNTIL (rv # NIL) OR (frame = NIL) OR Ty.IsQualified(n);
   RETURN rv
END FindVarLoc;

PROCEDURE FindVar*(m: Module; frame: Frame; n: Ty.QualName): TypeSym;
VAR ignore: Frame;
BEGIN
   RETURN FindVarLoc(m, frame, n, ignore)
END FindVar;


(* Returns a typesym assoctiated with the name "n".  Could be
   a type, procedure or var. *)
PROCEDURE FindAny*(m: Module; scope: Frame; n: Ty.QualName): TypeSym;
VAR rv: TypeSym;
    ignore: Frame;
BEGIN
   REPEAT
      rv := FindTypeThisFrame(m, scope, n);
      IF rv = NIL THEN rv := FindProcThisFrame(m, scope, n) END;
      IF rv = NIL THEN rv := FindVarLocThisFrame(m, scope, n, ignore) END;
      IF rv = NIL THEN rv := FindConstThisFrame(m, scope, n) END;
      scope := scope.searchNext
   UNTIL (rv # NIL) OR (scope = NIL) OR Ty.IsQualified(n);
   RETURN rv
END FindAny;

PROCEDURE EvalTerminal*(cur: Module; scan: Lex.T; t: Ast.Terminal; 
                       VAR dest: ConstVal): Ast.SrcError;
VAR rv: Ast.SrcError;
    buf: ARRAY 64 OF CHAR;
BEGIN
   rv := NIL;
   IF (t.tok.kind = Lex.ConstInt) OR (t.tok.kind = Lex.ConstHex) THEN
      Lex.Extract(scan, t.tok, buf);
      Cvt.StringToInt(buf, dest.ival, dest.bval);
      IF dest.bval THEN
         dest.kind := KInteger
      ELSE
         rv := Ast.MkSrcError("Could not parse int literal", scan, t)
      END
   ELSIF (t.tok.kind = Lex.ConstString) & (t.tok.len = 3) THEN
      (* Single char strings are treated as character literals *)
      dest.cval := scan.buf[t.tok.start+1];
      dest.kind := KChar;
   ELSIF t.tok.kind = Lex.ConstHexString THEN
      (* A character constant in hex. *) 
      Lex.Extract(scan, t.tok, buf);
      buf[t.tok.len-1] := "H";  (* Replace X so StringToInt recognizes as hex *)
      Cvt.StringToInt(buf, dest.ival, dest.bval);
      IF dest.bval THEN
         IF (dest.ival < 0) OR (dest.ival > 255) THEN
            rv := Ast.MkSrcError("Char literal out of range.", scan, t)
         ELSE
            dest.cval := CHR(dest.ival);
            dest.kind := KChar;
         END
      ELSE
         rv := Ast.MkSrcError("Could not parse hex char literal", scan, t)
      END      
   ELSIF t.tok.kind = Lex.ConstReal THEN
      Lex.Extract(scan, t.tok, buf);
      Cvt.StringToReal(buf, dest.rval, dest.bval);
      IF dest.bval THEN
         dest.kind := KReal
      ELSE
         rv := Ast.MkSrcError("Could not parse real literal", scan, t)
      END      
   ELSIF (t.tok.kind = Lex.KTRUE) OR (t.tok.kind = Lex.KFALSE) THEN
      dest.kind := KBoolean;
      dest.bval := t.tok.kind = Lex.KTRUE
   ELSE
      rv := Ast.MkSrcError("Unrecognized constant value", scan, t)
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
                         VAR dest: ConstVal): Ast.SrcError;
VAR rv: Ast.SrcError;
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
               rv := Ast.MkSrcError("Bad type for negation", scan, t)
            END
         END
      ELSIF br.kind = Ast.BkBinOp THEN
         op := Ast.TermAt(br, 1);
         rv := EvalConstExpr(cur, frame, scan, Ast.GetChild(br, 0), lhs);
         IF rv = NIL THEN
            rv := EvalConstExpr(cur, frame, scan, Ast.GetChild(br, 2), rhs);
            IF rv = NIL THEN
               IF lhs.kind # rhs.kind THEN
                  rv := Ast.MkSrcError("Mismatched binary operand types.", scan, 
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
                        rv := Ast.MkSrcError("+ not defined for types", scan, op)
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
                        rv := Ast.MkSrcError("- not defined for types", scan, op)
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
                        rv := Ast.MkSrcError("* not defined for types", scan, op)
                     END     
                  ELSE
                     rv := Ast.MkSrcError("Unrecognized binary op", scan, op)
                  END
               END
            END
         END
      ELSIF br.kind = Ast.BkQualIdent THEN
         Ty.GetQualName(br, scan, qname);
         IF ~LookupConst(cur, frame, qname, dest) THEN
            any := Ast.GetChild(br, 1);
            rv := Ast.MkSrcError("Unknown constant", scan, any(Ast.Terminal))
         END
      ELSIF br.kind = Ast.BkDesignator THEN
         IF br.childLen = 1 THEN
            rv := EvalConstExpr(cur, frame, scan, Ast.GetChild(br, 0), dest);
         ELSE
            rv := Ast.MkSrcError("Can not evaluate as const expression", scan, br)
         END
      ELSE
         rv := Ast.MkSrcError("Unrecognized constant value", scan, br)
      END
   ELSE
      ASSERT(FALSE)
   END
   RETURN rv
END EvalConstExpr;

PROCEDURE TyCvtQualIdent(mod: Module; frame: Frame; br: Ast.Branch; scan: Lex.T; 
                         VAR rv: Ty.Type): Ast.SrcError;
VAR err: Ast.SrcError;
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
         err := Ast.MkSrcError("Could not find type", scan, br)
      END
   ELSE
      err := Ast.MkSrcError("Could not find type in imported module", scan, br)
   END
   RETURN err
END TyCvtQualIdent;

PROCEDURE TyCvtArrayType(mod: Module; frame: Frame; br: Ast.Branch; scan: Lex.T; 
                         VAR rv: Ty.Type): Ast.SrcError;
VAR err: Ast.SrcError;
    art, lastarr: Ty.ArrayType;
    cv: ConstVal;
    dms: Ast.Branch;
    x: Ast.T;
    i, ndims: INTEGER;
BEGIN
   dms := Ast.BranchAt(br, Ast.ArrayTypeDims);
   ndims := dms.childLen;
   lastarr := NIL;
   (* expand ARRAY 1,2,3 OF X to nested arrays ARRAY 1 OF ARRAY 2 ... *)
   FOR i := ndims-1 TO 0 BY -1 DO
      art := Ty.MkArrayType();
      x := Ast.GetChild(dms, i);
      err := EvalConstExpr(mod, frame, scan, x, cv);
      IF err = NIL THEN
         IF cv.kind = KInteger THEN
            art.dim := cv.ival
         ELSE
            err := Ast.MkSrcError("Array dim not integer", scan, x)
         END
      END;
      IF lastarr = NIL THEN
         (* innermost type, get the contained type *)
         err := CvtStrucType(mod, frame, Ast.GetChild(br, Ast.ArrayTypeType),
                             scan, art.ty);   
         lastarr := art
      ELSE
         art.ty := lastarr;
         lastarr := art
      END
   END;
   IF err # NIL THEN
      rv := NIL
   ELSE
      rv := art
   END;

   RETURN err
END TyCvtArrayType;

(* For a FormalParameters node, creates Ty.ProcParam for all 
   parameters and links them into "paramList". Returns the 
   list in reverse order, caller can reverse it if it cares. *)
PROCEDURE TyCvtFormalParams(mod: Module; frame: Frame; formalParams: Ast.Branch; scan: Lex.T;
                            VAR paramList: Ty.ProcParam): Ast.SrcError;
VAR err: Ast.SrcError;
    i, j, k: INTEGER;
    name: Ast.Terminal;
    fpsec, formalType: Ast.Branch;
    paramTy: Ty.Type;
    fld: Ty.ProcParam;
    arrType: Ty.ArrayType;
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
         (* For each level of openarray nesting, add an outer array
            type that is marked as an open array. Propagate flags
            up from the inner type. *)
         FOR k := 0 TO formalType.n-1 DO
            arrType := Ty.MkArrayType();   
            arrType.dim := 0;
            INCL(arrType.flags, Ty.OpenArray);
            arrType.flags := arrType.flags + paramTy.flags;
            arrType.ty := paramTy;
            paramTy := arrType
         END;
         FOR j := 0 TO fpsec.childLen-2 DO
            fld := Ty.MkProcParam();
            name := Ast.TermAt(fpsec, j);
            fld.seekpos := name.tok.start;
            Lex.Extract(scan, name.tok, fld.name);
            fld.ty := paramTy;
            fld.next := paramList;
            paramList := fld
         END
      END;
      INC(i)
   END;
   RETURN err
END TyCvtFormalParams;


PROCEDURE TyCvtProc(mod: Module; frame: Frame; br: Ast.Branch; scan: Lex.T;
                    VAR rv: Ty.Type): Ast.SrcError;
VAR err: Ast.SrcError;
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
      ts := MkTypeSym(tsProcParam);
      ts.id := param.seekpos;
      Strings.Append(param.name, ts.name);
      ts.ty := param.ty;
      ts.next := rv.vars;
      rv.vars := ts;
      param := param.next
   END;
   RETURN rv
END FrameForParams;

PROCEDURE TyCvtProcDecl(mod: Module; frame: Frame; procDecl: Ast.Branch; scan: Lex.T;
                        VAR rv: Ty.Type): Ast.SrcError;
VAR err: Ast.SrcError;
    pty: Ty.ProcType;
    formalParams, bodyDecls, retType: Ast.Branch;
BEGIN
   err := NIL;
   pty := Ty.MkProcType();
   formalParams := Ast.BranchAt(procDecl, Ast.ProcedureDeclParams);
   pty.body := Ast.BranchAt(procDecl, Ast.ProcedureDeclBody);
   IF formalParams # NIL THEN
      retType := Ast.BranchAt(formalParams, Ast.FormalParamsReturn);
      IF retType # NIL THEN
         err := CvtStrucType(mod, frame, retType, scan, pty.returnTy)
      END;
   END;
   IF (err = NIL) & (formalParams # NIL) THEN
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
                       VAR rv: Ty.Type): Ast.SrcError;
VAR err: Ast.SrcError;
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
      IF (pt.ty.kind = Ty.KRecord) OR (pt.ty.kind = Ty.KAny) THEN
         rv := pt
      ELSE
         err := Ast.MkSrcError("Pointer only point to records", scan, br)
      END
   END

   RETURN err
END TyCvtPointer;

PROCEDURE TyCvtRecord(mod: Module; frame: Frame; br: Ast.Branch; scan: Lex.T; 
                      VAR rv: Ty.Type): Ast.SrcError;
VAR err: Ast.SrcError;
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
      err := CvtStrucType(mod, frame, base, scan, rt.base);
      IF (rt.base # NIL) & (rt.base.kind # Ty.KRecord) THEN
         err := Ast.MkSrcError("Records must extend other records", 
                               scan, base)
      END
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
PROCEDURE CvtStrucTypeImpl(mod: Module; frame: Frame; t: Ast.T; scan: Lex.T; VAR rv: Ty.Type): Ast.SrcError;
VAR err: Ast.SrcError;
    term: Ast.Terminal;
    br: Ast.Branch;
BEGIN
   IF t IS Ast.Terminal THEN
      err := Ast.MkSrcError("Unrecognized type", scan, term)
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
         err := Ast.MkSrcError("Unrecognized type", scan, br) 
      END
   END
   RETURN err
END CvtStrucTypeImpl;

PROCEDURE MkConstant(): TypeSym;
VAR rv: TypeSym;
BEGIN
   rv := MkTypeSym(tsConst);
   NEW(rv.val);
   rv.val.kind := KError;
   RETURN rv
END MkConstant; 

PROCEDURE AddErr*(mod: Module; err: Ast.SrcError); 
BEGIN
   IF mod.nofErrs < LEN(mod.errs) THEN
      mod.errs[mod.nofErrs] := err^;
      INC(mod.nofErrs)
   END
END AddErr;


(* Creates TypeSyms for variables in a VarDeclaration and
   links them into the list pointed to by varList *)
PROCEDURE CvtAndLinkVars(mod: Module; frame: Frame; varDecl: Ast.Branch; 
                         scan: Lex.T; VAR varList: TypeSym): Ast.SrcError;
VAR err: Ast.SrcError;
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
         fv := MkTypeSym(tsVar);
         name := Ast.TermAt(identList, i);
         fv.id := name.tok.start;
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
    err: Ast.SrcError;
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
    cd: TypeSym;
    br: Ast.Branch;
    id: Ast.Terminal;
    err: Ast.SrcError;
BEGIN
   IF consts # NIL THEN
      FOR i := 0 TO consts.childLen-1 DO
         cd := MkConstant();
         br := Ast.BranchAt(consts, i);
         ASSERT(br.kind = Ast.BkConstDeclaration);
         id := Ast.TermAt(br, 0);
         Lex.Extract(scan, id.tok, cd.name);
         cd.id := id.tok.start;
         cd.export := id.export;
         err := EvalConstExpr(rv, frame, scan, Ast.GetChild(br, 1), cd.val^);
         IF err # NIL THEN
            AddErr(rv, err)
         ELSE
            cd.ty := Ty.PrimitiveType(cd.val.kind);
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
    err: Ast.SrcError;
    fname: Ast.Terminal;
    procDecl: Ast.Branch;
BEGIN
   IF procDecls # NIL THEN
      FOR i := 0 TO procDecls.childLen-1 DO
         procDecl := Ast.BranchAt(procDecls, i);
         ent := MkTypeSym(tsProc);
         fname := Ast.TermAt(procDecl, Ast.ProcedureDeclName);
         ent.id := fname.tok.start;
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
    err: Ast.SrcError;
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
               err := Ast.MkSrcError("Can not resolve pointer target to type",
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

PROCEDURE MkSrcName(ty: Ty.Type);
BEGIN
   NEW(ty.srcName);
   ty.srcName.module[0] := 0X;
   ty.srcName.name[0] := 0X
END MkSrcName;

(* Loads all of the types from "types" into "frame" *)
PROCEDURE LoadTypes(types: Ast.Branch; mod: Module; frame: Frame; scan: Lex.T);
VAR br: Ast.Branch;
    i: INTEGER;
    ty: TypeSym;
    t: Ast.Terminal;
    err: Ast.SrcError;
BEGIN
   IF types # NIL THEN
      ASSERT(types.kind = Ast.BkTypeDeclSeq);
      FOR i := 0 TO types.childLen-1 DO
         br := Ast.BranchAt(types, i);
         ASSERT(br.kind = Ast.BkTypeDeclaration);
         ty := MkTypeSym(tsType);
         err := CvtStrucType(mod, frame, 
                             Ast.GetChild(br, Ast.TypeDeclVal), scan,
                                          ty.ty);
         IF err = NIL THEN
            t := Ast.TermAt(br, Ast.TypeDeclName);
            Lex.Extract(scan, t.tok, ty.name);
            ty.id := t.tok.start;
            ty.export := t.export;
            MkSrcName(ty.ty);
            Strings.Append(mod.name, ty.ty.srcName.module);
            Strings.Append(ty.name, ty.ty.srcName.name);
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

PROCEDURE WriteConstVal(VAR w: BinWriter.T; cv: ConstVal);
BEGIN
   BinWriter.I8(w, cv.kind);
   CASE cv.kind OF
      KByte: BinWriter.U8(w, cv.byval)
      |KInteger: BinWriter.I32(w, cv.ival)
      |KBoolean: BinWriter.Bool(w, cv.bval)
      |KReal: BinWriter.F32(w, cv.rval)
      |KChar: BinWriter.I32(w, ORD(cv.cval))
      |KSet: BinWriter.I32(w, SYSTEM.VAL(INTEGER, cv.setval)) 
   END;
END WriteConstVal;

PROCEDURE ReadConstVal(VAR w: BinReader.T; VAR cv: ConstVal);
VAR i: INTEGER;
BEGIN
   BinReader.I8(w, cv.kind);
   CASE cv.kind OF
      KByte: BinReader.U8(w, cv.byval)
      |KInteger: BinReader.I32(w, cv.ival)
      |KBoolean: BinReader.Bool(w, cv.bval)
      |KReal: BinReader.F32(w, cv.rval)
      |KChar: BinReader.I32(w, i); cv.cval := CHR(i)
      |KSet: BinReader.I32(w, i); cv.setval := SYSTEM.VAL(SET, i)
   END;
END ReadConstVal;

PROCEDURE WriteTypeSyms(VAR w: BinWriter.T; VAR ss: Ty.SerialState;
                        ts: TypeSym);
BEGIN
   WHILE ts # NIL DO
      IF ts.export THEN
         BinWriter.Bool(w, TRUE);
         BinWriter.I8(w, ts.kind);
         BinWriter.String(w, ts.name);
         BinWriter.I32(w, ts.id);
         Ty.Write(w, ss, ts.ty);
         IF BinWriter.Cond(w, ts.val # NIL) THEN
            WriteConstVal(w, ts.val^)
         END
      END;
      ts := ts.next
   END;
   (* Terminate with false entry *)
   BinWriter.Bool(w, FALSE)
END WriteTypeSyms;

PROCEDURE ReadTypeSyms(VAR w: BinReader.T; VAR ss: Ty.SerialState): TypeSym;
VAR rv, ts: TypeSym;
BEGIN
   rv := NIL;
   WHILE BinReader.Cond(w) DO
      ts := MkTypeSym(0);
      BinReader.I8(w, ts.kind);
      BinReader.String(w, ts.name);
      BinReader.I32(w, ts.id);
      ts.ty := Ty.Read(w, ss);
      IF BinReader.Cond(w) THEN
         NEW(ts.val);
         ReadConstVal(w, ts.val^)
      END;
      ts.next := rv;
      rv := ts
   END;
   RETURN rv
END ReadTypeSyms;

PROCEDURE WriteFrame(VAR w: BinWriter.T; VAR ss: Ty.SerialState;
                     fr: Frame);
BEGIN
   BinWriter.I32(w, FrameMagic);
   BinWriter.I32(w, SYSTEM.VAL(INTEGER, fr.flags));
   WriteTypeSyms(w, ss, fr.types);
   WriteTypeSyms(w, ss, fr.constants);
   WriteTypeSyms(w, ss, fr.vars);
   WriteTypeSyms(w, ss, fr.procedures);
END WriteFrame;

PROCEDURE ReadFrame(VAR w: BinReader.T; VAR ss: Ty.SerialState;
                    mod: Module): Frame;
VAR fr: Frame;
    mag, flags: INTEGER;
BEGIN
   BinReader.I32(w, mag); ASSERT(mag = FrameMagic);
   BinReader.I32(w, flags);
   fr := MkFrame(mod, NIL);
   fr.flags := SYSTEM.VAL(SET, flags);
   fr.types := ReadTypeSyms(w, ss);
   fr.constants := ReadTypeSyms(w, ss);
   fr.vars := ReadTypeSyms(w, ss);
   fr.procedures := ReadTypeSyms(w, ss);
   RETURN fr
END ReadFrame;

PROCEDURE WriteImportList(VAR w: BinWriter.T; mod: Module);
   (* Writes just the names of the imported modules. 
      Just used to chase down dependencies. *)
VAR m: Module;
BEGIN
   m := mod.imports;
   WHILE BinWriter.Cond(w, m # NIL) DO
      BinWriter.String(w, m.name);
      m := m.importNext
   END
END WriteImportList;

PROCEDURE ReadImportList(VAR w: BinReader.T; mod: Module);
   (* Reads a skeleton import list.  The modules are empty, 
      and just have the import names, enough for 
      dependency chasing. *)
VAR m: Module;
BEGIN
   mod.imports := NIL;
   WHILE BinReader.Cond(w) DO
      NEW(m);
      BinReader.String(w, m.name);
      m.importNext := mod.imports;
      mod.imports := m
   END
END ReadImportList; 

(* Writes out a symbol table that contains just the public members.
   This acts as an interface file that we can load to resolve
   names and types for imported modules when compiling *)
PROCEDURE Write*(VAR w: BinWriter.T; mod: Module);
VAR ss: Ty.SerialState;
BEGIN
   Ty.InitSerialState(ss);
   BinWriter.String(w, FileMagic);
   BinWriter.I32(w, SymtabVer);
   BinWriter.String(w, mod.name);
   WriteImportList(w, mod);
   WriteFrame(w, ss, mod.frame);
END Write;

(* Reads the previously saved symtab for a module.  Contains just
   the exported items *)
PROCEDURE Read*(VAR w: BinReader.T): Module;
VAR mod: Module;
    mag: ARRAY 5 OF CHAR;
    ver: INTEGER;
    ss: Ty.SerialState;
BEGIN
   Ty.InitSerialState(ss);
   mod := MkModule();
   BinReader.String(w, mag);
   ASSERT(Ast.StringEq(mag, FileMagic));
   BinReader.I32(w, ver);
   ASSERT(ver = SymtabVer);
   BinReader.String(w, mod.name);
   mod.localAlias := mod.name;
   ReadImportList(w, mod);
   mod.frame := ReadFrame(w, ss, mod);
   RETURN mod
END Read;

(* If not loaded already, get the builtin symbols *)
PROCEDURE LoadBuiltins();
VAR path: Path.T;
    rd: BinReader.T;
    ts: TypeSym;
BEGIN
   IF Builtins = NIL THEN
      Config.BuiltinsSymtabFile(path);
      IF BinReader.Init(rd, path.str) THEN
         Builtins := Read(rd);
         BinReader.Finish(rd);
         ts := Builtins.frame.procedures;
         WHILE ts # NIL DO
            INCL(ts.ty.flags, Ty.Builtin);
            ts := ts.next
         END
      ELSE
         (* Fatal for most cases, but we allow it to continue.
            This is necessary when bootstrapping and creating
            the initial stubs in lib/stubs *)
         Dbg.S("WARNING: could not load builtins symtab");
         Dbg.Ln;
      END
   END
END LoadBuiltins;

PROCEDURE LoadImports(imps: Ast.Branch; rv: Module; scan: Lex.T);
VAR i: INTEGER;
    imp: Ast.Branch;
    rd: BinReader.T;
    spath: Path.T;
    mod: Module;
    modname: ARRAY 64 OF CHAR;
    mname: Ast.Terminal;
BEGIN
   IF imps # NIL THEN
      ASSERT(imps.kind = Ast.BkImportList);
      FOR i := 0 TO imps.childLen-1 DO
         imp := Ast.BranchAt(imps, i);
         mname := Ast.TermAt(imp, 1);
         Lex.Extract(scan, mname.tok, modname);
         IF Config.FindModPath(modname, spath) THEN
            IF BinReader.Init(rd, spath.str) THEN
               mod := Read(rd);
               BinReader.Finish(rd);
               mname := Ast.TermAt(imp, 0);
               IF mname # NIL THEN
                  (* Alias *)
                  Lex.Extract(scan, mname.tok, mod.localAlias)
               END;
               mod.importNext := rv.imports;
               rv.imports := mod;
            ELSE
               AddErr(rv, Ast.MkSrcError(
                  "Could not read symbol file for module", 
                  scan, mname))
            END
         ELSE
            AddErr(rv, Ast.MkSrcError(
               "Could not find symbol file for module", 
               scan, mname))
         END;
      END
   END
END LoadImports;


(* Create a Module for the AST version of a Module.   Populates the 
   errs/nofErrs fields of the module if there's errors *)
PROCEDURE BuildModule*(ast: Ast.T; scan: Lex.T): Module;
VAR rv: Module;
    mod, initBlock: Ast.Branch;
    term: Ast.Terminal;
    initBlockTs: TypeSym;
    ptype: Ty.ProcType;
BEGIN
   LoadBuiltins();
   rv := MkModule();
   IF Builtins # NIL THEN 
      rv.frame.searchNext := Builtins.frame;  (* Always in scope *)
   END;
   mod := ast(Ast.Branch);
   ASSERT(mod.kind = Ast.BkModule);
   term := Ast.TermAt(mod, Ast.ModuleName);
   Lex.Extract(scan, term.tok, rv.name);
   LoadImports(Ast.BranchAt(mod, Ast.ModuleImports), rv, scan);
   IF rv.nofErrs = 0 THEN
      LoadDecls(Ast.BranchAt(mod, Ast.ModuleDecls), rv, rv.frame, scan)
   END;
   initBlock := Ast.BranchAt(mod, Ast.ModuleInit);
   IF initBlock # NIL THEN
      (* make a frame and proctype for the module init code *)
      rv.initBlock := MkFrame(rv, rv.frame);
      initBlockTs := MkTypeSym(tsProc);
      ptype := Ty.MkProcType();
      ptype.body := Ast.MkProcedureBody();
      Ast.AddChild(ptype.body, NIL);
      Ast.AddChild(ptype.body, initBlock);
      Ast.AddChild(ptype.body, NIL);
      initBlockTs.ty := ptype;
      initBlockTs.frame := rv.frame;
      rv.initBlock.procedures := initBlockTs
   END;

   RETURN rv   
END BuildModule;

BEGIN
   CvtStrucType := CvtStrucTypeImpl;
   LoadDecls := LoadDeclsImpl;
   FileMagic := "SYMT";
   Builtins := NIL;
END Symtab.
