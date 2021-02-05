MODULE TestSymtab;
IMPORT
   Ast, Dbg, Par:=Parser, Semcheck, Symtab, Ty:=Types;
VAR
   buf: ARRAY 356 OF CHAR;
   par: Par.T;
   ast: Ast.T;
   mod: Symtab.Module;
   i: INTEGER;
   cc: Symtab.Constant;
   qn: Ty.QualName;
   scstate: Semcheck.State;
   DbgPrintFrame: PROCEDURE(frame: Symtab.Frame; indent: INTEGER; 
                            showProcFrame: BOOLEAN);

PROCEDURE DbgPrintTypeSym(ts: Symtab.TypeSym; indent: INTEGER; 
                          showScope: BOOLEAN);
BEGIN
   WHILE ts # NIL DO
      Dbg.Ind(indent); Dbg.S(ts.name); 
      IF ts.export THEN Dbg.S("*") END;
      Dbg.Ln;
      Ty.DbgPrint(ts.ty, indent+1);
      Dbg.Ln;
      IF (ts.frame # NIL) & showScope THEN
         Dbg.Ind(indent); Dbg.S("SCOPE FRAME"); Dbg.Ln;
         DbgPrintFrame(ts.frame, indent+2, showScope);
         Dbg.Ln; Dbg.Ln
      END;
      ts := ts.next
   END;
   Dbg.Ln
END DbgPrintTypeSym;

PROCEDURE DbgPrintFrameImpl(frame: Symtab.Frame; indent: INTEGER; 
                        showProcFrame: BOOLEAN);
   PROCEDURE Section(heading: ARRAY OF CHAR; syms: Symtab.TypeSym; 
                     indent: INTEGER; showProcFrame: BOOLEAN);
   BEGIN
      IF syms # NIL THEN
         Dbg.Ln; Dbg.Ind(indent);  Dbg.S(heading);  Dbg.Ln;
         DbgPrintTypeSym(syms, indent+1, showProcFrame);
      END;
   END Section;
BEGIN
   (* TODO consts *)
   Dbg.Ln;Dbg.Ind(indent);Dbg.S("FRAME");
   IF Symtab.ffProcParams IN frame.flags THEN Dbg.S(" ffProcParams") END;
   IF Symtab.ffModule IN frame.flags THEN Dbg.S(" ffModule") END;
   Section("*** TYPES", frame.types, indent, FALSE);
   Section("*** VARS", frame.vars, indent, FALSE);
   Section("*** PROCEDURES", frame.procedures, indent, TRUE);
END DbgPrintFrameImpl;

BEGIN
   DbgPrintFrame := DbgPrintFrameImpl;
   par := Par.NewFromFile("../test/files/Borb.mod");
   ast := Par.ParseModule(par);

   mod := Symtab.BuildModule(ast, par.scan);
   Dbg.S("DONE "); Dbg.S(mod.name); Dbg.Ln; Dbg.S(" numerrs: "); Dbg.I(mod.nofErrs); 
   Dbg.Ln;

   Semcheck.Init(scstate, mod, par.scan);
   IF Semcheck.RunPass0(scstate) THEN
      ast.ops.toStr(ast, par.scan.buf, 0);
      qn.module := "";
      qn.name := "x";
      cc := Symtab.FindConst(mod, mod.frame, qn);
      IF cc # NIL THEN
         Dbg.S("X = "); Dbg.I(cc.val.ival); Dbg.Ln
      END;

      DbgPrintFrame(mod.frame, 0, TRUE);
      Dbg.Ln
   ELSE
      Dbg.S("Semcheck does not love you");Dbg.Ln;
   END;
   FOR i := 0 TO mod.nofErrs-1 DO
      Symtab.Announce(mod.errs[i], par.curFile)
   END;
END TestSymtab.
