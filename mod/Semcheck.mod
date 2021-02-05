(* Semantic checking and desugaring passes for the AST *)
MODULE Semcheck;
IMPORT
   Ast, Dbg, Symtab, Lex := Scanner, Ty:=Types;

TYPE
   State* = RECORD
      mod: Symtab.Module;
      scan: Lex.T
   END;

PROCEDURE Init*(VAR s: State; mod: Symtab.Module; scan: Lex.T);
BEGIN
   s.mod := mod;
   s.scan := scan
END Init;

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

END Semcheck.
