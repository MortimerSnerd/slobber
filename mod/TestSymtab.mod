MODULE TestSymtab;
IMPORT
   Ast, Dbg, Par:=Parser, Symtab, Ty:=Types;
VAR
   buf: ARRAY 356 OF CHAR;
   par: Par.T;
   ast: Ast.T;
   mod: Symtab.Module;
   i: INTEGER;
   cc: Symtab.Constant;
   qn: Ty.QualName;

PROCEDURE DbgPrintTypeSym(ts: Symtab.TypeSym; indent: INTEGER);
BEGIN
   WHILE ts # NIL DO
      Dbg.Ind(indent); Dbg.S(ts.name); 
      IF ts.export THEN Dbg.S("*") END;
      Dbg.Ln;
      Ty.DbgPrint(ts.ty, indent+1);
      Dbg.Ln;
      ts := ts.next
   END;
   Dbg.Ln
END DbgPrintTypeSym;

BEGIN
   buf := "MODULE Borb; CONST x*= 2 + 4 - 1; TYPE PBog* = POINTER TO Bogon; Toady = ARRAY 5 OF CHAR; PapaBogon = RECORD name*: CHAR END; Bogon = RECORD(PapaBogon) dogs: REAL; easy, cheese: INTEGER END; CB* = PROCEDURE(a: ARRAY OF PBog; VAR c, beach: REAL): INTEGER; VAR a, b, c: REAL; nod: Toady;  END Borb.";
   par := Par.NewFromFile("../test/files/Borb.mod");
   ast := Par.ParseModule(par);
   ast.ops.toStr(ast, par.scan.buf, 0);

   mod := Symtab.BuildModule(ast, par.scan);

   Dbg.S("DONE "); Dbg.S(mod.name); Dbg.Ln; Dbg.S(" numerrs: "); Dbg.I(mod.nofErrs); 
   Dbg.Ln;
   FOR i := 0 TO mod.nofErrs-1 DO
      Symtab.Announce(mod.errs[i], par.curFile)
   END;
   qn.module := "";
   qn.name := "x";
   cc := Symtab.FindConst(mod, mod.frame, qn);
   IF cc # NIL THEN
      Dbg.S("X = "); Dbg.I(cc.val.ival); Dbg.Ln
   END;

   Dbg.S("***Types"); Dbg.Ln;
   DbgPrintTypeSym(mod.frame.types, 1);

   Dbg.Ln; Dbg.S("***vars"); Dbg.Ln;
   DbgPrintTypeSym(mod.frame.vars, 1);

   Dbg.Ln; Dbg.S("***procs"); Dbg.Ln;
   DbgPrintTypeSym(mod.frame.procedures, 1);

   Dbg.Ln
END TestSymtab.
