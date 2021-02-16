(* Semantic checking and desugaring passes for the AST *)
MODULE Semcheck;
IMPORT
   Ast, Dbg, Symtab, Lex := Scanner, Ty:=Types;

TYPE
   State* = RECORD
      mod: Symtab.Module;
      scan: Lex.T
   END;

VAR
   (* Fwd decls *)
   Pass1StmtSeq: PROCEDURE(st: State; proc: Symtab.TypeSym; seq: Ast.Branch);

PROCEDURE Init*(VAR s: State; mod: Symtab.Module; scan: Lex.T);
BEGIN
   s.mod := mod;
   s.scan := scan
END Init;

PROCEDURE AddErr(st: State; msg: ARRAY OF CHAR; loc: Ast.T);
BEGIN
   Symtab.AddErr(st.mod, Ast.MkSrcError(msg, st.scan, loc))
END AddErr;

(* Steps through a designator to see what the end type will be once all
   of the selectors are applied. Adds type annotations to the selectors
   so the calculated information can be re-used later. *)
PROCEDURE DesignatorType(mod: Symtab.Module; scope: Symtab.Frame; 
                         desig: Ast.Branch; scan: Lex.T; 
                         VAR err: Ast.SrcError): Ty.Type;
VAR qn: Ty.QualName;
    lhs, rv: Ty.Type;
    lhsTs: Symtab.TypeSym;
    ix: INTEGER;
    sel, desigQIdent: Ast.Branch;
    fld: Ty.RecordField;
    name: Ast.Terminal;
    lastAnswer: Ty.TypeNote;
BEGIN
   (* TODO - we're ignoring export rules for external types, vars and 
      procs, and record fields *)
   lastAnswer := Ty.Remember(Ast.GetChild(desig, desig.childLen-1));
   IF lastAnswer # NIL THEN
      rv := lastAnswer.ty;
      err := NIL
   ELSE
      desigQIdent :=  Ast.BranchAt(desig, Ast.DesignatorQIdent);
      Ty.GetQualName(desigQIdent, scan, qn);
      lhsTs := Symtab.FindAny(mod, scope, qn);
      IF lhsTs = NIL THEN
         err := Ast.MkSrcError("Could not find name.", scan, desig);
         rv := Ty.ErrorType
      ELSE
         Ty.Note(desigQIdent, lhsTs.ty);
         lhs := lhsTs.ty;
         ix := 1;
         err := NIL;
         rv := Ty.ErrorType;
         WHILE (err = NIL) & (ix < desig.childLen) DO
            sel := Ast.BranchAt(desig, ix); 
            IF sel.kind = Ast.BkSelector THEN
               CASE sel.n OF
               Ast.FieldAccess:
                  lhs := Ty.DerefPointers(lhs);
                  IF lhs.kind = Ty.KRecord THEN
                     name := Ast.TermAt(sel, 0);
                     fld := Ty.FindField(lhs(Ty.RecordType), scan, name.tok);
                     IF fld # NIL THEN
                        lhs := fld.ty
                     ELSE
                        err := Ast.MkSrcError("Unknown field", scan, sel)
                     END 
                  ELSE
                     err := Ast.MkSrcError("Identifier not a record type.", 
                                           scan, desig)
                  END

               |Ast.ArrayAccess:
                  IF lhs.kind = Ty.KArray THEN
                     lhs := lhs(Ty.ArrayType).ty              
                  ELSE
                     err := Ast.MkSrcError("Not an array.", scan, sel)
                  END

               |Ast.PtrDeref:
                  IF lhs.kind = Ty.KPointer THEN
                     lhs := lhs(Ty.PointerType).ty
                  ELSE
                     err := Ast.MkSrcError("De-ref'd type not a pointer.", 
                                           scan, desig)
                  END

               |Ast.TypeGuard:
                  Ty.GetQualName(Ast.BranchAt(sel, 0), scan, qn);
                  lhsTs := Symtab.FindType(mod, scope, qn);
                  IF lhsTs # NIL THEN
                     lhs := lhsTs.ty
                  ELSE
                     err := Ast.MkSrcError("Unknown type", scan, sel)
                  END
               END;
               IF err = NIL THEN
                  Ty.Note(sel, lhs)
               END
            ELSE
               err := Ast.MkSrcError("BUG: bad selector?", scan, sel)
            END;
            INC(ix)
         END
      END;
      IF (err = NIL) & (lhs # NIL) THEN
         rv := lhs
      END;
   END;
   RETURN rv
END DesignatorType;

(* Returns the type of the selector indexed by "selIx" in the 
   designator *)
PROCEDURE DesignatorSelectorType(mod: Symtab.Module; scope: Symtab.Frame; 
                                 desig: Ast.Branch; selIx: INTEGER; 
                                 scan: Lex.T; VAR err: Ast.SrcError): Ty.Type;
VAR rv: Ty.Type;
    note: Ty.TypeNote;
BEGIN
   (* Take advantage of the fact that DesignatorType memoizes the 
      selector types as it goes, so this doesn't cost us much if
      it's already been done on this designator *)
   rv := DesignatorType(mod, scope, desig, scan, err);
   note := Ty.Remember(Ast.BranchAt(desig, selIx));
   IF note # NIL THEN
      rv := note.ty
   ELSE
      rv := NIL
   END;
   RETURN rv
END DesignatorSelectorType;

PROCEDURE IsCompare(tk: Lex.TokKind): BOOLEAN;
   RETURN (Lex.tfRelational IN Lex.TokenFlags[tk])
          OR (tk = Lex.KOR) OR (tk = Lex.AMPERSAND)
END IsCompare;

PROCEDURE NthCallParam(t: Ast.Branch; n: INTEGER): Ast.T;
   (* If t is a call, and has a nth parameter, returns
      it.  Returns NIL otherwise *)
VAR rv: Ast.T;
    expLst: Ast.Branch;
BEGIN
   IF t.kind = Ast.BkCall THEN
      expLst := Ast.BranchAt(t, Ast.CallParams);
      IF (expLst # NIL) & (expLst.childLen > n) THEN
         rv := Ast.GetChild(expLst, n)
      ELSE
         rv := NIL
      END 
   ELSE
      rv := NIL
   END
   RETURN rv
END NthCallParam;

PROCEDURE IsSystemValCall(calldesig: Ast.Branch; scan: Lex.T): BOOLEAN;
VAR rv: BOOLEAN;
    qident: Ast.Branch;
    qn: Ty.QualName;
BEGIN
   IF calldesig.childLen = 1 THEN
      qident := Ast.BranchAt(calldesig, Ast.DesignatorQIdent);
      Ty.GetQualName(qident, scan, qn);
      rv := Ast.StringEq(qn.module, "SYSTEM");
   ELSE
      rv := FALSE
   END
   RETURN rv
END IsSystemValCall;

(* Returns the type for an expression.  If the ast reprensents something
   typeless, like an IF statement, returns a KVoid type.  If there's
   an error in the typing (like a binop with different types of arguments), 
   then returns a KTypeError, and populates "err" with a user presentable
   error. Do not modify the returned types. *)
PROCEDURE ExpressionType(mod: Symtab.Module; scope: Symtab.Frame; 
                         t: Ast.T; scan: Lex.T; 
                         VAR err: Ast.SrcError): Ty.Type;
VAR rv, lhs, rhs: Ty.Type;
    term: Ast.Terminal;
    br, calldesig: Ast.Branch;
    procTy: Ty.ProcType;
    param: Ast.T;
BEGIN
   rv := Ty.VoidType;
   IF t IS Ast.Terminal THEN
      term := t(Ast.Terminal);
      rv := Ty.TypeForTerminal(term.tok)
   ELSE
      br := t(Ast.Branch);
      IF br.kind = Ast.BkUnOp THEN
         rv := ExpressionType(mod, scope, Ast.GetChild(br, 1), scan, err)
      ELSIF br.kind = Ast.BkSet THEN
         rv := Ty.PrimitiveType(Ty.KSet)
      ELSIF br.kind = Ast.BkParenExpr THEN
         rv := ExpressionType(mod, scope, Ast.GetChild(br, 0), scan, err)
      ELSIF br.kind = Ast.BkBinOp THEN
         term := Ast.TermAt(br, 1);
         IF term.tok.kind = Lex.COLEQ THEN
            (* The type checking for assigments is left up to 
               CheckAssignment() *)
            rv := Ty.VoidType
         ELSIF (term.tok.kind = Lex.KIS) OR (term.tok.kind = Lex.KIN) THEN
            (* Just boolean.  We don't check the types here, CheckExpression
               will do that just once *)
            rv := Ty.PrimitiveType(Ty.KBoolean)
         ELSE
            lhs := ExpressionType(mod, scope, Ast.GetChild(br, 0), scan, err);
            IF err = NIL THEN
               rhs := ExpressionType(mod, scope, Ast.GetChild(br, 2), scan, err);
               IF err = NIL THEN
                  IF Ty.Equal(lhs, rhs) THEN
                     IF IsCompare(term.tok.kind) THEN
                        rv := Ty.BooleanType
                     ELSE
                        rv := lhs
                     END
                  ELSE
                     rv := Ty.ErrorType;
                     err := Ast.MkSrcError("Binary operator mismatched types", 
                                           scan, Ast.GetChild(br, 1))
                  END
               END
            END
         END
      ELSIF br.kind = Ast.BkCall THEN
         (* We only check the expression type here, not whether
            the call parameters are properly typed. The one expression
            is for SYSTEM.VAL, where the return type is the type named
            in the first parameter. *)
         calldesig := Ast.BranchAt(br, Ast.CallDesignator);
         lhs := ExpressionType(mod, scope, calldesig, scan, err);
         IF lhs.kind = Ty.KProcedure THEN
            procTy := lhs(Ty.ProcType);
            IF procTy.returnTy = NIL THEN
               rv := Ty.VoidType
            ELSE
               IF (procTy.returnTy.kind = Ty.KAny) 
                     & IsSystemValCall(calldesig, scan) THEN
                  param := NthCallParam(br, 0);
                  rv := NIL;
                  IF (param # NIL) & (param IS Ast.Branch) THEN
                     rv := Ty.AcceptPrimTyName(
                        Ast.BranchAt(param(Ast.Branch), 0), scan);
                  END;
                  IF rv = NIL THEN
                     err := Ast.MkSrcError(
                        "Expecting primitive type name", scan, param)
                  END;
                  IF err = NIL THEN
                     param := NthCallParam(br, 1);
                     IF param = NIL THEN
                        err := Ast.MkSrcError(
                           "VAL needs two parameters.", scan, calldesig)
                     ELSE
                        lhs := ExpressionType(mod, scope, param, scan, err);
                        IF ~Ty.IsPrimitive(lhs) THEN
                           err := Ast.MkSrcError(
                              "2nd argument of VAL should be a primitive type",
                              scan, param)
                        END
                     END
                  END
               ELSE
                  rv := procTy.returnTy;
               END
            END
         ELSE
            err := Ast.MkSrcError("LHS of '(' is not a function type.", 
                                  scan, br);
            rv := Ty.ErrorType
         END

      ELSIF br.kind = Ast.BkDesignator THEN
         rv := DesignatorType(mod, scope, br, scan, err)
      END
   END
   RETURN rv
END ExpressionType; 
      
PROCEDURE FixupDesignator(st: State; proc: Symtab.TypeSym; desig: Ast.Branch);
VAR qn: Ty.QualName;
    nsel, qname: Ast.Branch;
BEGIN
   qname := Ast.BranchAt(desig, Ast.DesignatorQIdent);
   Ty.GetQualName(qname, st.scan, qn);
   IF Ty.IsQualified(qn) & (Symtab.FindImport(st.mod, qn.module) = NIL) THEN
      (* parsed as a module.something, but is really an unqualified name
         with a "something" selector. 
         a.b field c -> nil a field b field c *)
         nsel := Ast.MkSelector(Ast.FieldAccess);
         Ast.AddChild(nsel, Ast.GetChild(qname, 1));
         Ast.SetChild(qname, 1, Ast.GetChild(qname, 0));
         Ast.SetChild(qname, 0, NIL);
         Ast.InsertChild(desig, Ast.DesignatorSelectors, nsel)
   END
END FixupDesignator;

PROCEDURE LastIsTypeGuard(br: Ast.Branch): BOOLEAN;
VAR rv: BOOLEAN;
    li: Ast.Branch;
BEGIN
   IF (br.kind = Ast.BkDesignator) & (br.childLen > 1) THEN
      li := Ast.BranchAt(br, br.childLen-1);
      rv := (li.kind = Ast.BkSelector) & (li.n = Ast.TypeGuard)
   ELSE
      rv := FALSE
   END
   RETURN rv
END LastIsTypeGuard; 

(* Child n of br is a designator.  See if it's a function
   call that's been mistaken for a TypeGuard:
      x := Something(x) 
      [binop x := [desig [nil Something] [tguard x]]
   IF Something is a function, then rewrite to:
      [binop x := [call [desig [nil Something]] [ExpList [desig [nil x]]]]]

   Note that the following is invalid if Something is a function, because
   the grammar doesn't support field access directly on a FunctionCall(x).y:
      x := Something(x).y *)
PROCEDURE FixupTyGuardFunctions(st: State; proc: Symtab.TypeSym; 
                                br: Ast.Branch; n: INTEGER);
VAR desig, tguard, param, newCall, callParams, parmDesig: Ast.Branch;
    lhsTy: Ty.Type;
    err: Ast.SrcError;
BEGIN
   desig := Ast.BranchAt(br, n);
   IF LastIsTypeGuard(desig) THEN
      lhsTy := DesignatorSelectorType(st.mod, proc.frame, desig,
                                      desig.childLen - 2, st.scan, err);
      IF (lhsTy # NIL) & (lhsTy.kind = Ty.KProcedure) THEN
         tguard := Ast.BranchAt(desig, desig.childLen-1);
         param := Ast.BranchAt(tguard, 0);
         Ast.DropLast(desig);
         parmDesig := Ast.MkDesignator();
         Ast.AddChild(parmDesig, param);
         callParams := Ast.MkExpList();
         Ast.AddChild(callParams, parmDesig);
         newCall := Ast.MkCall();
         Ast.AddChild(newCall, desig);
         Ast.AddChild(newCall, callParams);
         Ast.SetChild(br, n, newCall)
      END
   END
END FixupTyGuardFunctions;

PROCEDURE Pass0Proc(st: State; proc: Symtab.TypeSym); 
   PROCEDURE Walk(st: State; proc: Symtab.TypeSym; br: Ast.Branch); 
   VAR i: INTEGER;
       n: Ast.T;
       chld: Ast.Branch;
   BEGIN
      IF br.kind = Ast.BkDesignator THEN
         FixupDesignator(st, proc, br)
      ELSE
         FOR i := 0 TO br.childLen-1 DO
            n := Ast.GetChild(br, i);
            IF n IS Ast.Branch THEN
               chld := n(Ast.Branch);
               IF chld.kind = Ast.BkDesignator THEN
                  FixupDesignator(st, proc, chld);
                  FixupTyGuardFunctions(st, proc, br, i);
                  chld := Ast.BranchAt(br, i)
               END;
               Walk(st, proc, chld)
            END
         END
      END
   END Walk;
BEGIN
   Walk(st, proc, proc.ty(Ty.ProcType).body)
END Pass0Proc;

(* Desugaring and AST fixup pass that uses type information to
   disambiguate parts of the parse tree that are wrong *)   
PROCEDURE RunPass0*(st: State): BOOLEAN;
VAR it: Symtab.TSIter;
    proc: Symtab.TypeSym;
BEGIN
   IF st.mod.nofErrs = 0 THEN
      it := Symtab.IterAllProcs(st.mod);
      REPEAT
         proc := it.op.Next(it);
         IF proc # NIL THEN
            Pass0Proc(st, proc)
         END
      UNTIL proc = NIL;
   END;
   RETURN st.mod.nofErrs = 0
END RunPass0;

PROCEDURE CheckCall(st: State; proc: Symtab.TypeSym; call: Ast.Branch);
VAR i: INTEGER;
    desig, expList: Ast.Branch;
    paramExp: Ast.T;
    pt: Ty.ProcType;
    param: Ty.ProcParam;
    lhsTy: Ty.Type;
    expTy: Ty.Type;
    done: BOOLEAN;
    err: Ast.SrcError;
BEGIN
   (* It should be a proc or a var with a procedure type *)
   desig := Ast.BranchAt(call, Ast.CallDesignator);
   lhsTy := DesignatorType(st.mod, proc.frame, desig, st.scan, err);
   IF err # NIL THEN
      Symtab.AddErr(st.mod, err)
   ELSIF lhsTy.kind # Ty.KProcedure THEN
      AddErr(st, "Not a procedure type.", desig)
   ELSE
      pt := lhsTy(Ty.ProcType);
      param := pt.params;
      expList := Ast.BranchAt(call, Ast.CallParams);
      done := FALSE;
      i := 0;
      WHILE ~done & (i < expList.childLen) DO
         paramExp := Ast.GetChild(expList, i);
         IF param = NIL THEN
            AddErr(st, "Too many parameters passed to procedure.", 
                   paramExp);
            done := TRUE       
         ELSE
            IF param.ty.kind = Ty.KPrimName THEN
               expTy := Ty.AcceptPrimTyName(Ast.BranchAt(paramExp(Ast.Branch), 0),
                                            st.scan);
               IF expTy = NIL THEN
                  AddErr(st, "Expecting a primitive type name.",
                         paramExp)
               END
            ELSE
               expTy := ExpressionType(st.mod, proc.frame, paramExp, 
                                       st.scan, err);
               IF err # NIL THEN
                  Symtab.AddErr(st.mod, err)
               END;
               IF ~Ty.Equal(expTy, param.ty) 
                     & ~Ty.IsSubtype(expTy, param.ty) THEN
                  AddErr(st, "Mismatched proc parameter type.", paramExp)
               END;
            END;
            param := param.next;
            INC(i)
         END
      END;
      IF param # NIL THEN
         AddErr(st, "Not enough parameters passed to procedure.", 
                expList)
      END;
   END;
END CheckCall;

PROCEDURE CheckIsExpr(st: State; proc: Symtab.TypeSym; expr: Ast.Branch);
VAR lty: Ty.Type;
    rty: Symtab.TypeSym;
    err: Ast.SrcError;
    qn: Ty.QualName;
    rhs: Ast.Branch;
BEGIN
   lty := ExpressionType(st.mod, proc.frame, Ast.GetChild(expr, 0), 
                         st.scan, err);
   IF err = NIL THEN 
      rhs := Ast.BranchAt(expr, 2);
      IF rhs.kind = Ast.BkDesignator THEN
         Ty.GetQualName(Ast.BranchAt(rhs, Ast.DesignatorQIdent), 
                        st.scan, qn);
         rty := Symtab.FindType(st.mod, proc.frame, qn);
         IF rty # NIL THEN
            IF ~Ty.IsSubtype(rty.ty, lty) THEN
               AddErr(st, "Can not be true, types are disjoint",
                      Ast.GetChild(expr, 1))
            END
         ELSE
            AddErr(st, "Type not found.", rhs)
         END
      ELSE
         AddErr(st, "Expecting a type designator on RHS of IS", rhs)
      END;
   ELSE
      Symtab.AddErr(st.mod, err)
   END
END CheckIsExpr;

PROCEDURE CheckInExpr(st: State; proc: Symtab.TypeSym; expr: Ast.Branch);
VAR ty: Ty.Type;
    err: Ast.SrcError;    
BEGIN
   ty := ExpressionType(st.mod, proc.frame, Ast.GetChild(expr, 0), 
                        st.scan, err);
   IF err = NIL THEN
      IF Ty.IsIntConvertible(ty.kind) THEN
         ty := ExpressionType(st.mod, proc.frame, Ast.GetChild(expr, 2), 
                              st.scan, err);
         IF (err = NIL) & (ty.kind # Ty.KSet) THEN
            AddErr(st, "RHS of IN must be a SET", 
                   Ast.GetChild(expr, 2))
         END
      ELSE
         AddErr(st, "LHS of IN must be a integer.", expr)
      END
   END;
   IF err # NIL THEN
      Symtab.AddErr(st.mod, err)   
   END;
END CheckInExpr;

(* Recursively walk expression checking any calls.  Type checking for
   operators is already done at this point *)
PROCEDURE CheckExpression(st: State; proc: Symtab.TypeSym; expr: Ast.T);
VAR i: INTEGER;
    br: Ast.Branch;
    operator: Ast.Terminal;
BEGIN
   IF expr IS Ast.Branch THEN
      br := expr(Ast.Branch);
      IF br.kind = Ast.BkCall THEN
         CheckCall(st, proc, br)
      ELSIF br.kind = Ast.BkBinOp THEN
         operator := Ast.TermAt(br, 1);
         IF operator.tok.kind = Lex.KIS THEN
            CheckIsExpr(st, proc, br)
         ELSIF operator.tok.kind = Lex.KIN THEN
            CheckInExpr(st, proc, br)
         END 
      END;
      FOR i := 0 TO br.childLen-1 DO
         CheckExpression(st, proc, Ast.GetChild(br, i))
      END
   END
END CheckExpression;

(* Gets the int value of a label.  If it's a character, converts it to an 
   int  *)
PROCEDURE GetLabelInt(st: State; t: Ast.Terminal; VAR err: Ast.SrcError;
                      VAR dest: Symtab.ConstVal);
BEGIN
   err := Symtab.EvalTerminal(st.mod, st.scan, t, dest);
   IF err = NIL THEN
      IF dest.kind = Symtab.KChar THEN
         dest.ival := ORD(dest.cval)
      ELSIF dest.kind # Symtab.KInteger THEN
         err := Ast.MkSrcError("Label should be int or character.", st.scan, 
                                t)
      END
   END
END GetLabelInt;

PROCEDURE CheckCaseConstant(st: State; proc: Symtab.TypeSym; lab: Ast.T);
VAR ts: Symtab.TypeSym;
    qn: Ty.QualName;
BEGIN
   Ty.GetQualName(lab(Ast.Branch), st.scan, qn); 
   ts := Symtab.FindConst(st.mod, proc.frame, qn);
   IF ts = NIL THEN
      AddErr(st, "Could not find constant.", lab)
   ELSIF ~Ty.IsIntConvertible(ts.val.kind) 
            & (ts.val.kind # Ty.KChar) THEN
      AddErr(st, "Expecting a integer or char constant.", 
             lab)
   END
END CheckCaseConstant;

PROCEDURE CheckCases(st: State; proc: Symtab.TypeSym; stmt: Ast.Branch);
VAR iter: Ast.CaseIterator;
    body, labelRange, case: Ast.Branch;
    lab: Ast.T;
    lval, rval: Symtab.ConstVal;
    err: Ast.SrcError;
BEGIN
   Ast.IterateCases(iter, stmt);
   WHILE Ast.HasAnotherCase(iter) DO
      case := Ast.NextCase(iter);
      body := Ast.BranchAt(case, Ast.CaseStmtSeq);
      WHILE Ast.HasAnotherLabelRange(iter) DO
         labelRange := Ast.NextLabelRange(iter);
         lab := Ast.GetChild(labelRange, 0);
         IF lab IS Ast.Branch THEN
            CheckCaseConstant(st, proc, lab)
         ELSE
            IF labelRange.childLen > 1 THEN
               GetLabelInt(st, lab(Ast.Terminal), err, lval);
               IF err # NIL THEN
                  Symtab.AddErr(st.mod, err)
               ELSE
                  lab := Ast.GetChild(labelRange, 1);
                  IF lab IS Ast.Branch THEN
                     CheckCaseConstant(st, proc, lab)
                  ELSE
                     GetLabelInt(st, lab(Ast.Terminal), err, rval);
                     IF err # NIL THEN
                        Symtab.AddErr(st.mod, err)
                     ELSE
                        IF rval.ival < lval.ival THEN
                           AddErr(st, "RHS of label range must be >= LHS", lab)
                        END
                     END
                  END
               END
            END
         END
      END
      (* TODO warn about coverage for a type, overlaps and holes *)
   END
END CheckCases;

PROCEDURE CheckRecordCases(st: State; proc: Symtab.TypeSym; stmt: Ast.Branch);
VAR ts: Symtab.TypeSym;
    iter: Ast.CaseIterator;
    case, labelRange, body: Ast.Branch;
    lab: Ast.T;
    qn: Ty.QualName;
BEGIN
   Ast.IterateCases(iter, stmt);
   WHILE Ast.HasAnotherCase(iter) DO
      case := Ast.NextCase(iter);
      WHILE Ast.HasAnotherLabelRange(iter) DO
         labelRange := Ast.NextLabelRange(iter);
         IF labelRange.childLen > 1 THEN
            AddErr(st, "Can't have range for record type case.", 
                   labelRange)
         ELSE
            lab := Ast.GetChild(labelRange, 0);
            IF lab IS Ast.Terminal THEN
               AddErr(st, "Expecting types name for record case label.", 
                      lab)
            ELSE
               Ty.GetQualName(lab(Ast.Branch), st.scan, qn);
               ts := Symtab.FindType(st.mod, proc.frame, qn);
               IF ts = NIL THEN
                  AddErr(st, "Expecting types name for record case label.", 
                         lab)
               END;
               (* TODO - fill out symtab information so we can detect 
                         whether the case can never be hit for the
                         case's expression type.  Need more info in 
                         Type.Ty so we have the original qualified type
                         name for the types. Right now the types are 
                         flattened out into trees with no indication
                         if any of the pieces have names *)
            END
         END;
      END;
      body := Ast.BranchAt(case, Ast.CaseStmtSeq);
      Pass1StmtSeq(st, proc, body)
   END
END CheckRecordCases;

(* The parser ensures the case label are integers, strings or qualidents, 
   so we just need to sanity check the usage *) 
PROCEDURE CheckCaseStmt(st: State; proc: Symtab.TypeSym; stmt: Ast.Branch);
VAR expr: Ast.Branch;
    err: Ast.SrcError;
    ety: Ty.Type;
BEGIN
   expr := Ast.BranchAt(stmt, Ast.CaseStmtExpr);
   CheckExpression(st, proc, expr);
   ety := ExpressionType(st.mod, proc.frame, expr, st.scan, err);
   IF err = NIL THEN
      IF Ty.IsRecord(ety) OR Ty.IsRecordRef(ety) THEN
         CheckRecordCases(st, proc, stmt)
      ELSIF Ty.IsInteger(ety) OR Ty.IsCharLiteral(ety) THEN
         CheckCases(st, proc, stmt)
      ELSE
         AddErr(st, "Case expr must be int, char or record type.", 
                expr)
      END
   ELSE  
      Symtab.AddErr(st.mod, err)
   END;
END CheckCaseStmt;

PROCEDURE ExprShouldBeBoolean(st: State; proc: Symtab.TypeSym; 
                              expr: Ast.T);
VAR err: Ast.SrcError;
    ty: Ty.Type;
BEGIN
   IF expr # NIL THEN
      ty := ExpressionType(st.mod, proc.frame, expr, st.scan, err);
      IF err # NIL THEN
         Symtab.AddErr(st.mod, err)
      ELSIF ty.kind # Ty.KBoolean THEN
         AddErr(st, "Should be BOOLEAN expression", expr)
      END;
   END
END ExprShouldBeBoolean;

PROCEDURE ExprShouldBeInteger(st: State; proc: Symtab.TypeSym; 
                              expr: Ast.T);
VAR err: Ast.SrcError;
    ty: Ty.Type;
BEGIN
   IF expr # NIL THEN
      ty := ExpressionType(st.mod, proc.frame, expr, st.scan, err);
      IF err # NIL THEN
         Symtab.AddErr(st.mod, err)
      ELSIF ty.kind # Ty.KInteger THEN
         AddErr(st, "Should be INTEGER expression", expr)
      END;
   END
END ExprShouldBeInteger;

PROCEDURE CheckIfStmt(st: State; proc: Symtab.TypeSym; stmt: Ast.Branch);
VAR i: INTEGER;
   cond: Ast.Branch;
BEGIN
   FOR i := 0 TO stmt.childLen-1 BY 2 DO
      cond := Ast.BranchAt(stmt, i);
      ExprShouldBeBoolean(st, proc, cond);
      CheckExpression(st, proc, cond);
      Pass1StmtSeq(st, proc, Ast.BranchAt(stmt, i+1))
   END
END CheckIfStmt;

PROCEDURE CheckWhileStmt(st: State; proc: Symtab.TypeSym; stmt: Ast.Branch);
VAR expr: Ast.Branch;
    i: INTEGER;
BEGIN
   FOR i := 0 TO stmt.childLen-1 BY 2 DO
      expr := Ast.BranchAt(stmt, i);
      ExprShouldBeBoolean(st, proc, expr);
      CheckExpression(st, proc, expr);
      Pass1StmtSeq(st, proc, Ast.BranchAt(stmt, i+1))
   END
END CheckWhileStmt;

PROCEDURE CheckRepeatStmt(st: State; proc: Symtab.TypeSym; stmt: Ast.Branch);
VAR expr: Ast.T;
BEGIN
   expr := Ast.GetChild(stmt, Ast.RepeatStmtCond);
   ExprShouldBeBoolean(st, proc, expr);
   CheckExpression(st, proc, expr);
   Pass1StmtSeq(st, proc, Ast.BranchAt(stmt, Ast.RepeatStmtBody))
END CheckRepeatStmt;

PROCEDURE CheckForStmt(st: State; proc: Symtab.TypeSym; stmt: Ast.Branch);
VAR vname: Ast.Terminal;
    vsym: Symtab.TypeSym;
    qn: Ty.QualName;
    expr: Ast.T;
BEGIN
   vname := Ast.TermAt(stmt, Ast.ForStmtVarName);
   Ty.GetUnqualName(vname, st.scan, qn);
   vsym := Symtab.FindVar(st.mod, proc.frame, qn);
   IF vsym = NIL THEN
      AddErr(st, "Unknown var name.", vname);
   END;
   expr := Ast.GetChild(stmt, Ast.ForStmtBegin);
   ExprShouldBeInteger(st, proc, expr);
   CheckExpression(st, proc, expr);
   expr := Ast.GetChild(stmt, Ast.ForStmtEnd);
   ExprShouldBeInteger(st, proc, expr);
   CheckExpression(st, proc, expr);
   expr := Ast.GetChild(stmt, Ast.ForStmtBy);
   ExprShouldBeInteger(st, proc, expr);
   CheckExpression(st, proc, expr);
   Pass1StmtSeq(st, proc, Ast.BranchAt(stmt, Ast.ForStmtBody))
END CheckForStmt;

PROCEDURE IsLocWritable(st: State; proc: Symtab.TypeSym; 
                        desig: Ast.Branch): BOOLEAN;
VAR i: INTEGER;
    dident: Ast.Branch; 
    sel: Ast.Branch;
    done, rv: BOOLEAN;
    locFrame: Symtab.Frame;
    varInf: Symtab.TypeSym;
    qn: Ty.QualName;
    note: Ty.TypeNote;
    ignore: Ty.Type;
    err: Ast.SrcError;
BEGIN
   (* Find out var identifier the designator starts out with 
      is writable. This can influence whether the result of
      other selectors is writable *)
   rv := TRUE;
   dident := Ast.BranchAt(desig, Ast.DesignatorQIdent);
   Ty.GetQualName(dident, st.scan, qn);
   varInf := Symtab.FindVarLoc(st.mod, proc.frame, qn, locFrame);
   IF varInf # NIL THEN
      IF Ty.IsQualified(qn) THEN
         rv := FALSE;  (* Can not write imported module vars *)
      ELSIF Symtab.ffProcParams IN locFrame.flags THEN
         (* Special cases for writing to parameters. Structured 
            types are assumed passed as pointers rather than copied,
            so no writes are allowed.  By value parameters and VAR
            parameters are fine to write to.i *)
         rv := ~Ty.IsStructured(varInf.ty) OR (Ty.Var IN varInf.ty.flags)
      END;
      (* Walk selectors.  If we do field acces on a pointer, 
         the resulting loc is definitely writable. *)
      i := Ast.DesignatorSelectors;
      done := FALSE;
      (* Make sure the designator already has selector notes *)
      ignore := DesignatorType(st.mod, proc.frame, desig, st.scan, err);
      IF err # NIL THEN 
         Symtab.AddErr(st.mod, err);
         done := TRUE
      END;
      WHILE ~done & (i < desig.childLen) DO
         sel := Ast.BranchAt(desig, i);
         IF sel.n = Ast.FieldAccess THEN
            (* qident and selectors have type notes for the type
               of evaluating them.  So the note for i-1 is
               the type before the current selector is applied *)
            note := Ty.Remember(Ast.GetChild(desig, i-1));
            IF note.ty.kind = Ty.KPointer THEN
               rv := TRUE;
               done := TRUE
            END
         ELSE
            (* Other selectors don't change the writability
               of what came before them *)
         END;
         INC(i);
      END
   ELSE
      (* Not a var? *)
      AddErr(st, "Location can not be writable", desig);
      rv := FALSE
   END;
   RETURN rv
END IsLocWritable;

(* If we get here, we know tyl and tyr are not exactly equal. 
   We check to see if it's one of the array assignments that
   are allowable, or if it's just an error *)
PROCEDURE CheckAssignmentValid(st: State; proc: Symtab.TypeSym; 
                               lhs, rhs: Ast.T; tyl, tyr: Ty.Type);
VAR larr, rarr: Ty.ArrayType;
    nomatch: BOOLEAN;
BEGIN
   nomatch := FALSE;
   IF (tyl.kind = Ty.KArray) & (tyr.kind = Ty.KArray) THEN
      larr := tyl(Ty.ArrayType);
      rarr := tyr(Ty.ArrayType);
      IF Ty.Equal(larr.ty, rarr.ty) THEN
         IF ~(Ty.OpenArray IN larr.flags) & (rarr.dim > larr.dim) THEN
            AddErr(st, "LHS of assignment not big enough to hold RHS.", 
                   rhs)
         END
      ELSE
         nomatch := TRUE
      END
   ELSIF (tyl.kind = Ty.KPointer) & (tyr.kind = Ty.KPointer) THEN
      IF ~Ty.IsSubtype(tyr, tyl) THEN
         AddErr(st, "RHS is not a subtype of LHS of assignment", 
                lhs)
      END
   ELSE
      nomatch := TRUE;
   END;
   IF nomatch THEN
      AddErr(st, "Assignment types do not match.", lhs)
   END
END CheckAssignmentValid;

(* We need to typecheck the assignment and and make sure the LHS is
   a writable var. We have to be more flexible than just a 
   Types.Equal, since arrays of different sizes can be assigned
   as a shorthand for COPY. *)
PROCEDURE CheckAssignment(st: State; proc: Symtab.TypeSym; stmt: Ast.Branch);
VAR desig: Ast.Branch;
    rhs: Ast.T;
    tyl, tyr: Ty.Type;
    err: Ast.SrcError;
BEGIN
   desig := Ast.BranchAt(stmt, 0);
   rhs := Ast.GetChild(stmt, 2);
   IF ~IsLocWritable(st, proc, desig) THEN
      AddErr(st, "LHS of assignment is not writable.", desig)
   ELSE
      CheckExpression(st, proc, rhs);
      tyl := DesignatorType(st.mod, proc.frame, desig, st.scan, err);
      IF err = NIL THEN
         tyr := ExpressionType(st.mod, proc.frame, rhs, st.scan, err);
         IF err = NIL THEN
            IF ~Ty.Equal(tyl, tyr) THEN
               CheckAssignmentValid(st, proc, desig, rhs, tyl, tyr)
            END
         ELSE
            Symtab.AddErr(st.mod, err)
         END
      ELSE
         Symtab.AddErr(st.mod, err)
      END
   END
   (* TODO - IF RHS is literal, check LHS type value range and see
             if it is out of range, ie:  someByte := 256 *)
END CheckAssignment;

PROCEDURE CheckCallStmt(st: State; proc: Symtab.TypeSym; stmt: Ast.Branch);
BEGIN
   (* Call in a statement position.  We already know it doesn't return
      anything (or that's already been flagged), so just do some normal
      expression checking on it *)
   CheckExpression(st, proc, stmt)
END CheckCallStmt;

PROCEDURE IsParameterlessCall(stmt: Ast.Branch; ty: Ty.Type): BOOLEAN;
   RETURN (stmt.kind = Ast.BkDesignator)
          & (ty.kind = Ty.KProcedure)
          & (ty(Ty.ProcType).params = NIL)
END IsParameterlessCall;

PROCEDURE CheckStatement(st: State; proc: Symtab.TypeSym; 
                         parent: Ast.Branch; stmtIx: INTEGER;
                         stmt: Ast.Branch);
VAR err: Ast.SrcError;
    ty: Ty.Type;
    ncall: Ast.Branch;
BEGIN
   (* statements should have a void return type *)
   ty := ExpressionType(st.mod, proc.frame, stmt, st.scan, err);
   IF err # NIL THEN
      Symtab.AddErr(st.mod, err)
      
   ELSIF ty.kind # Ty.KVoid THEN
      IF (ty.kind = Ty.KProcedure) & IsParameterlessCall(stmt, ty) THEN
         (* Ex: Out.Ln; It's parsed as just a designator, 
            rewrite this to be a call so it will be obvious to 
            later stages. *)
         ncall := Ast.MkCall();
         Ast.AddChild(ncall, stmt);
         Ast.AddChild(ncall, NIL);
         Ast.SetChild(parent, stmtIx, ncall)
      ELSE
         Symtab.AddErr(st.mod, 
                       Ast.MkSrcError("Expression value ignored.", st.scan,
                       stmt)) 
      END
   ELSE
      IF stmt.kind = Ast.BkIfStmt THEN
         CheckIfStmt(st, proc, stmt)
      ELSIF stmt.kind = Ast.BkCaseStatement THEN
         CheckCaseStmt(st, proc, stmt)
      ELSIF stmt.kind = Ast.BkWhileStatement THEN
         CheckWhileStmt(st, proc, stmt)
      ELSIF stmt.kind = Ast.BkRepeatStatement THEN
         CheckRepeatStmt(st, proc, stmt)
      ELSIF stmt.kind = Ast.BkForStatement THEN
         CheckForStmt(st, proc, stmt)
      ELSIF stmt.kind = Ast.BkBinOp THEN
         (* := is the only binary op that returns void right now *)
         CheckAssignment(st, proc, stmt) 
      ELSIF stmt.kind = Ast.BkCall THEN
         CheckCallStmt(st, proc, stmt)
      ELSE
         AddErr(st, "What the balls", stmt)
      END
   END;
END CheckStatement;

PROCEDURE Pass1StmtSeqImpl(st: State; proc: Symtab.TypeSym; seq: Ast.Branch);
VAR i: INTEGER;
BEGIN
   IF seq # NIL THEN
      IF seq.kind = Ast.BkStatementSeq THEN
         FOR i := 0 TO seq.childLen-1 DO
            CheckStatement(st, proc, seq, i, Ast.BranchAt(seq, i))
         END
      ELSE
         AddErr(st, "Not a statement sequence?", seq)
      END
   END;
END Pass1StmtSeqImpl;

PROCEDURE Pass1Proc(st: State; proc: Symtab.TypeSym); 
VAR procBody, seq: Ast.Branch;
    retExpr: Ast.T;
    ptype: Ty.ProcType;
    err: Ast.SrcError;
    rExpTy: Ty.Type;
BEGIN
   ptype := proc.ty(Ty.ProcType);
   procBody := ptype.body;
   seq := Ast.BranchAt(procBody, Ast.ProcBodyStmts);
   Pass1StmtSeq(st, proc, seq);
   retExpr := Ast.GetChild(procBody, Ast.ProcBodyReturn);
   IF ptype.returnTy = NIL THEN
      IF retExpr # NIL THEN
         AddErr(st, "Can't return expression in proper procedure", retExpr)
      END
   ELSE
      IF retExpr = NIL THEN
         AddErr(st, "Function must return value", seq)
      ELSE
         CheckExpression(st, proc, retExpr);
         rExpTy := ExpressionType(st.mod, proc.frame, retExpr, st.scan, err);
         IF err = NIL THEN
            IF ~Ty.Equal(ptype.returnTy, rExpTy) THEN
               AddErr(st, "Return type does not match return expression type.", 
                      retExpr)
            END
         ELSE
            Symtab.AddErr(st.mod, err)
         END
      END
   END
END Pass1Proc;

(* Semantic and type checking pass *)
PROCEDURE RunPass1*(st: State): BOOLEAN;
VAR it: Symtab.TSIter;
    proc: Symtab.TypeSym;
BEGIN
   IF st.mod.nofErrs = 0 THEN
      it := Symtab.IterAllProcs(st.mod);
      REPEAT
         proc := it.op.Next(it);
         IF proc # NIL THEN
            Pass1Proc(st, proc)
         END
      UNTIL proc = NIL;
   END;
   RETURN st.mod.nofErrs = 0
   (* TODO propagate "const expression" up from constant tokens through 
      operators, so later stages can just look at a flag in the AST to 
      detect constant expressions that can be evaluated at compile time. *)
END RunPass1;

(* Runs all semcheck passes.  Returns false if any checks
   failed *)
PROCEDURE Run*(st: State): BOOLEAN;
   RETURN RunPass0(st) & RunPass1(st)
END Run;

BEGIN
   Pass1StmtSeq := Pass1StmtSeqImpl
END Semcheck.

