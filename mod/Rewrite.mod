(* Module for doing rewrites on AST trees, to convert complicted
   features into more primitive operations *)
MODULE Rewrite;
IMPORT
   Ast, Cvt := extConvert, Dbg, Lex := Scanner, Strings, Sym := Symtab,
   Ty := Types;
    
CONST
   MaxDepth=128;
TYPE
   Bldr = RECORD
      (* For building ast trees from the bottom up *)
      stk: ARRAY MaxDepth OF Ast.T;
      sp: INTEGER;
      scan: Lex.T;
      proc: Sym.TypeSym;
      locNum: INTEGER
   END;

PROCEDURE Bld(VAR b: Bldr; proc: Sym.TypeSym; scan: Lex.T);
BEGIN
   b.sp := 0;
   b.scan := scan;
   b.proc := proc;
   b.locNum := 0
END Bld;

PROCEDURE Push(VAR b: Bldr; t: Ast.T);
BEGIN
   b.stk[b.sp] := t;
   INC(b.sp)
END Push;

PROCEDURE Pop(VAR b: Bldr): Ast.T;
BEGIN
   DEC(b.sp);
   RETURN b.stk[b.sp]
END Pop;

PROCEDURE Replace(VAR b: Bldr; t: Ast.T);
BEGIN
   b.stk[b.sp-1] := t
END Replace;

PROCEDURE Top(VAR b: Bldr): Ast.T;
   RETURN b.stk[b.sp-1]
END Top;

PROCEDURE Stmt(VAR bld: Bldr; slist: Ast.Branch);
   (* Pops the last piece off the stack and adds it to 
      the given statement list *)
BEGIN
   Ast.AddChild(slist, Pop(bld));
END Stmt;

PROCEDURE GenLocVar(VAR b: Bldr; ty: Ty.Type);
   (* Generates a new local variable in the proc
      with the given type, and pushes the designator for
      it onto the stack *)
VAR qident, desig: Ast.Branch;
    tok: Lex.Token;
    vts: Sym.TypeSym;
    done: BOOLEAN;
BEGIN
    vts := Sym.MkTypeSym(Sym.tsVar);
    Cvt.IntToString(b.locNum, vts.name, done);
    INC(b.locNum);
    ASSERT(done);
    Strings.Append("g", vts.name);
    vts.ty := ty;
    vts.next := b.proc.frame.vars;
    b.proc.frame.vars := vts;
    Lex.MkValTok(b.scan, Lex.Id, vts.name, tok);
    qident := Ast.MkQualIdent();
    Ast.AddChild(qident, NIL);
    Ast.AddChild(qident, Ast.MkTerminal(tok));
    Sym.Note(qident, vts);
    desig := Ast.MkDesignator();
    Ast.AddChild(desig, qident);
    Push(b, desig);
END GenLocVar;

PROCEDURE Designator(VAR bld: Bldr; numSelectors: INTEGER);
   (* Expects a QualIdent and "numSelectors" selectors
      on the stack. Replaces them with a designator
      built from those pieces *)
VAR desig: Ast.Branch;
    i, dloc: INTEGER;
BEGIN
   desig := Ast.MkDesignator();
   dloc := bld.sp-1-numSelectors;
   Ast.AddChild(desig, bld.stk[dloc]);
   FOR i := bld.sp-numSelectors TO bld.sp-1 DO
      Ast.AddChild(desig, bld.stk[i])
   END;
   bld.stk[dloc] := desig;
   bld.sp := dloc + 1
END Designator;

PROCEDURE MaybeWrapQualIdent(VAR bld: Bldr);
   (* IF the top of the stack is a bare QualIdent, turns it
      into a designator.  Otherwise, leaves it alone *)
VAR t: Ast.T;
BEGIN
   t := bld.stk[bld.sp-1];
   IF (t IS Ast.Branch) & (t(Ast.Branch).kind = Ast.BkQualIdent) THEN
      Designator(bld, 0)
   END
END MaybeWrapQualIdent;

PROCEDURE NamedDesignator(VAR b: Bldr; mod, name: ARRAY OF CHAR);
   (* Creates a designator for the given name and pushes it *)
VAR qi, desig: Ast.Branch;
    tok: Lex.Token;
    qn: Ty.QualName;
    ts: Sym.TypeSym;
BEGIN
   qi := Ast.MkQualIdent();
   IF mod[0] = 0X THEN
      Ast.AddChild(qi, NIL)
   ELSE
      Lex.MkValTok(b.scan, Lex.Id, mod, tok);
      Ast.AddChild(qi, Ast.MkTerminal(tok));
   END;
   Lex.MkValTok(b.scan, Lex.Id, name, tok);
   Ast.AddChild(qi, Ast.MkTerminal(tok));
   qn.module := mod; qn.name := name;
   ts := Sym.FindAny(b.proc.frame.owningModule, b.proc.frame, qn);
   ASSERT(ts # NIL);
   Sym.Note(qi, ts);
   desig := Ast.MkDesignator();
   Ast.AddChild(desig, qi);
   Push(b, desig)
END NamedDesignator;

PROCEDURE Paren(VAR bld: Bldr);
   (* Parenthesizes expr in bld *)
VAR br: Ast.Branch;
BEGIN
   br := Ast.MkParenExpr();
   Ast.AddChild(br, Top(bld));
   Replace(bld, br)
END Paren;

PROCEDURE BinaryOp(VAR bld: Bldr; tok: Lex.TokKind);
   (* Composes top two items with binary operator *)
VAR op: Lex.Token;
    br: Ast.Branch;
    lhs, rhs: Ast.T;
BEGIN
   rhs := Pop(bld);
   lhs := Pop(bld);
   Lex.MkTok(bld.scan, tok, op);
   br := Ast.MkBinOp();
   Ast.AddChild(br, lhs);
   Ast.AddChild(br, Ast.MkTerminal(op));
   Ast.AddChild(br, rhs);
   Push(bld, br)
END BinaryOp;

PROCEDURE LabelRangeTest(VAR bld: Bldr; var: Ast.T; lr: Ast.Branch; 
                         isRecordCase: BOOLEAN);
   (* Makes a test to see if "var" matches LabelRange "lr" *)
VAR fst, snd: Ast.T;
    desig: Ast.Branch;
BEGIN
   fst := Ast.GetChild(lr, 0);
   snd := Ast.GetChild(lr, 1);
   IF isRecordCase THEN
      IF snd = NIL THEN
         Push(bld, var); 
         Push(bld, fst);
         MaybeWrapQualIdent(bld);
         BinaryOp(bld, Lex.KIS)
      ELSE
         Dbg.S("ERROR: range for record case"); Dbg.Ln;
         ASSERT(FALSE)
      END
   ELSE
      IF snd = NIL THEN
         Push(bld, var);
         Push(bld, fst);  
         MaybeWrapQualIdent(bld);
         BinaryOp(bld, Lex.EQ)
      ELSE
         Push(bld, var);
         Push(bld, fst);
         MaybeWrapQualIdent(bld);
         BinaryOp(bld, Lex.GTE);
         Paren(bld);
         Push(bld, var);
         Push(bld, snd);
         MaybeWrapQualIdent(bld);
         BinaryOp(bld, Lex.LTE);
         Paren(bld);
         BinaryOp(bld, Lex.AMPERSAND)
      END
   END
END LabelRangeTest;

PROCEDURE LabelListTest(VAR bld: Bldr; var: Ast.T; cll: Ast.Branch; 
                        isRecordCase: BOOLEAN);
   (* Make a test to see if "var" matches any of the case label ranges
      in the case label list cll *)
VAR i: INTEGER;
    lr: Ast.Branch;
BEGIN
   FOR i := 0 TO cll.childLen-1 DO
      lr := Ast.BranchAt(cll, i);
      LabelRangeTest(bld, var, lr, isRecordCase);
      Paren(bld);
      IF i > 0 THEN
         BinaryOp(bld, Lex.KOR);
         Paren(bld)
      END
   END
END LabelListTest;

PROCEDURE Int(VAR bld: Bldr; n: INTEGER);
   (* Creates a int constant and pushes it *)
VAR te: Ast.Terminal;
    tok: Lex.Token;
    buf: ARRAY 25 OF CHAR;
    done: BOOLEAN;
BEGIN
   Cvt.IntToString(n, buf, done);
   ASSERT(done);
   Lex.MkValTok(bld.scan, Lex.ConstInt, buf, tok);
   te := Ast.MkTerminal(tok);
   Push(bld, te)
END Int;

PROCEDURE CallParams(VAR bld: Bldr; numParams: INTEGER);
   (* Pops the given number of parameters off the stack and
      replaces them with an ExprList, suitable for passing 
      to Call as the parameter list. Parameters should be
      pushed onto the stack in the same order as declared in
      the procedure to be called. *)
VAR i: INTEGER;
    el: Ast.Branch; 
BEGIN
   el := Ast.MkExpList();
   FOR i := bld.sp-numParams TO bld.sp-1 DO
      Ast.AddChild(el, bld.stk[i])
   END;
   bld.sp := bld.sp - (numParams - 1);
   bld.stk[bld.sp-1] := el
END CallParams;

PROCEDURE Call(VAR bld: Bldr);
   (* Expects a expression list and a designator on the 
      stack, replaces them with a procedure call *)
VAR ca: Ast.Branch;
BEGIN
   ca := Ast.MkCall();
   Ast.AddChild(ca, Pop(bld));
   Ast.AddChild(ca, Pop(bld));
   Push(bld, ca)
END Call;

PROCEDURE IfElse(VAR bld: Bldr; numConds: INTEGER);
   (* Expects "numConds" pairs and conditions on the 
      stack + 1 final body for the terminating else 
      clause.  Conditions are tested in the order they
      were pushed on the stack.  Replaces the conditions
      and bodies with the IF-ELSE statement chain *)
VAR i, elseBod, firstCond: INTEGER;
    if: Ast.Branch;
BEGIN
   if := Ast.MkIfStmt();
   (* Add all conditions and bodies except the terminating ELSE body *)
   elseBod := bld.sp - 1;
   firstCond := elseBod - numConds*2;
   Dbg.S("sp, elseBod, firstCond "); Dbg.I(bld.sp); Dbg.S(" ");
   Dbg.I(elseBod); Dbg.S(" "); Dbg.I(firstCond); Dbg.Ln;
   FOR i := firstCond TO elseBod -1 DO
      Ast.AddChild(if, bld.stk[i])
   END;
   Ast.AddChild(if, NIL); Ast.AddChild(if, bld.stk[elseBod]);
   bld.sp := firstCond + 1;
   bld.stk[firstCond] := if
END IfElse;

PROCEDURE CaseStmt*(mod: Sym.Module; proc: Sym.TypeSym; 
                    scan: Lex.T; stmt: Ast.Branch): Ast.Branch;
   (* Rewrites a case statement into an equivalent tree of IF statements. *)
VAR bld: Bldr;
    expr, exvar: Ast.T;
    note: Ty.TypeNote;
    stmts, caseNode, body, labelList, elsebod: Ast.Branch;
    iter: Ast.CaseIterator;
    count: INTEGER;
    isRecordCase: BOOLEAN;
BEGIN
   count := 0;
   isRecordCase := Ast.NfRecord IN stmt.flags;
   Bld(bld, proc, scan);
   expr := Ast.GetChild(stmt, Ast.CaseStmtExpr);
   stmts := Ast.MkStatementSeq();
   note := Ty.Remember(expr);
   GenLocVar(bld, note.ty);
   exvar := Top(bld); Push(bld, expr); BinaryOp(bld, Lex.COLEQ); Stmt(bld, stmts);
   Ast.IterateCases(iter, stmt); 
   (* Push if-cond body pairs for all clauses *)
   WHILE Ast.HasAnotherCase(iter) DO
      caseNode := Ast.NextCase(iter);
      body := Ast.BranchAt(caseNode, Ast.CaseStmtSeq);
      labelList := Ast.CurLabelList(iter);
      LabelListTest(bld, exvar, labelList, isRecordCase);
      Push(bld, body);
      INC(count);
   END;
   (* Push a statement sequence for the final else clause *)
   elsebod := Ast.MkStatementSeq();
   Int(bld, Ast.esadUnmatchedCase); CallParams(bld, 1);
   NamedDesignator(bld, "RUNTIME", "HALT"); Call(bld); 
   Stmt(bld, elsebod);
   Push(bld, elsebod);   
   (* Turn into giant if-elseif chain *)
   IfElse(bld, count); Stmt(bld, stmts); 
   RETURN stmts
END CaseStmt;

END Rewrite.
