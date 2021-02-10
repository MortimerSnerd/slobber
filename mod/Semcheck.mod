(* Semantic checking and desugaring passes for the AST *)
MODULE Semcheck;
IMPORT
   Ast, Symtab, Lex := Scanner, Ty:=Types;

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

PROCEDURE IsCompare(tk: Lex.TokKind): BOOLEAN;
   RETURN (Lex.tfRelational IN Lex.TokenFlags[tk])
          OR (tk = Lex.KOR) OR (tk = Lex.AMPERSAND)
END IsCompare;

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
    br: Ast.Branch;
    procTy: Ty.ProcType;
BEGIN
   rv := Ty.VoidType;
   IF t IS Ast.Terminal THEN
      term := t(Ast.Terminal);
      rv := Ty.TypeForTerminal(term.tok)
   ELSE
      br := t(Ast.Branch);
      IF br.kind = Ast.BkUnOp THEN
         rv := ExpressionType(mod, scope, Ast.GetChild(br, 1), scan, err)

      ELSIF br.kind = Ast.BkParenExpr THEN
         rv := ExpressionType(mod, scope, Ast.GetChild(br, 0), scan, err)

      ELSIF br.kind = Ast.BkBinOp THEN
         lhs := ExpressionType(mod, scope, Ast.GetChild(br, 0), scan, err);
         IF err = NIL THEN
            rhs := ExpressionType(mod, scope, Ast.GetChild(br, 2), scan, err);
            IF err = NIL THEN
               IF Ty.Equal(lhs, rhs) THEN
                  term := Ast.TermAt(br, 1);
                  IF term.tok.kind = Lex.COLEQ THEN
                     (* Assignment not typed *)
                     rv := Ty.VoidType
                  ELSIF IsCompare(term.tok.kind) THEN
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

      ELSIF br.kind = Ast.BkCall THEN
         (* We only check the expression type here, not whether
            the call parameters are properly typed *)
         lhs := ExpressionType(mod, scope, Ast.BranchAt(br, Ast.CallDesignator), 
                               scan, err);
         IF lhs.kind = Ty.KProcedure THEN
            procTy := lhs(Ty.ProcType);
            IF procTy.returnTy = NIL THEN
               rv := Ty.VoidType
            ELSE
               rv := procTy.returnTy
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
BEGIN
   desig := Ast.BranchAt(br, n);
   IF LastIsTypeGuard(desig) THEN
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
END FixupTyGuardFunctions;

PROCEDURE Pass0Proc(st: State; proc: Symtab.TypeSym); 
   PROCEDURE Walk(st: State; proc: Symtab.TypeSym; br: Ast.Branch); 
   VAR i: INTEGER;
       n: Ast.T;
   BEGIN
      IF br.kind = Ast.BkDesignator THEN
         FixupDesignator(st, proc, br)
      ELSE
         FOR i := 0 TO br.childLen-1 DO
            n := Ast.GetChild(br, i);
            IF n IS Ast.Branch THEN
               IF n(Ast.Branch).kind = Ast.BkDesignator THEN
                  FixupTyGuardFunctions(st, proc, br, i)
               END;
               Walk(st, proc, n(Ast.Branch))
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
            expTy := ExpressionType(st.mod, proc.frame, paramExp, 
                                    st.scan, err);
            IF err # NIL THEN
               Symtab.AddErr(st.mod, err)
            END;
            IF ~Ty.Equal(expTy, param.ty) THEN
               AddErr(st, "Mismatched proc parameter type.", paramExp)
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

(* Recursively walk expression checking any calls.  Type checking for
   operators is already done at this point *)
PROCEDURE CheckExpression(st: State; proc: Symtab.TypeSym; expr: Ast.T);
VAR i: INTEGER;
    br: Ast.Branch;
BEGIN
   IF expr IS Ast.Branch THEN
      br := expr(Ast.Branch);
      IF br.kind = Ast.BkCall THEN
         CheckCall(st, proc, br)
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
         IF ~(lab IS Ast.Terminal) THEN
            AddErr(st, "Was expecting a integer or char for the label.", lab)
         ELSE
            IF labelRange.childLen > 1 THEN
               GetLabelInt(st, lab(Ast.Terminal), err, lval);
               IF err # NIL THEN
                  Symtab.AddErr(st.mod, err)
               ELSE
                  lab := Ast.GetChild(labelRange, 1);
                  IF ~(lab IS Ast.Terminal) THEN
                     AddErr(st, "Was expecting a integer or char for the label.", lab)             
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
VAR expr: Ast.Branch;
BEGIN
   expr := Ast.BranchAt(stmt, Ast.RepeatStmtCond);
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

(* We' already know the types of both sides match by the time we get
   here, now just need to make sure it's a writable var.   *)
PROCEDURE CheckAssignment(st: State; proc: Symtab.TypeSym; stmt: Ast.Branch);
VAR desig: Ast.Branch;
BEGIN
   desig := Ast.BranchAt(stmt, 0);
   IF ~IsLocWritable(st, proc, desig) THEN
      AddErr(st, "LHS of assignment is not writable.", desig)
   END
END CheckAssignment;

PROCEDURE CheckCallStmt(st: State; proc: Symtab.TypeSym; stmt: Ast.Branch);
BEGIN
   (* Call in a statement position.  We already know it doesn't return
      anything (or that's already been flagged), so just do some normal
      expression checking on it *)
   CheckExpression(st, proc, stmt)
END CheckCallStmt;

PROCEDURE CheckStatement(st: State; proc: Symtab.TypeSym; stmt: Ast.Branch);
VAR err: Ast.SrcError;
    ty: Ty.Type;
BEGIN
   (* statements should have a void return type *)
   ty := ExpressionType(st.mod, proc.frame, stmt, st.scan, err);
   IF err # NIL THEN
      Symtab.AddErr(st.mod, err)
   ELSIF ty.kind # Ty.KVoid THEN
      Symtab.AddErr(st.mod, 
                    Ast.MkSrcError("Expression value ignored.", st.scan,
                    stmt)) 
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
            CheckStatement(st, proc, Ast.BranchAt(seq, i))
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

