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
   ty: Symtab.FrameVar;

BEGIN
   buf := "MODULE Borb; CONST x= 2 + 4 - 1; TYPE PBog = POINTER TO Bogon; Toady = ARRAY 5 OF CHAR; PapaBogon = RECORD name: CHAR END; Bogon = RECORD(PapaBogon) cheese: INTEGER END;  END Borb.";
   par := Par.NewFromString(buf);
   ast := Par.ParseModule(par);
   ast.ops.toStr(ast, par.scan.buf, 0);

   mod := Symtab.BuildModule(ast, par.scan);

   Dbg.S("DONE "); Dbg.S(mod.name); Dbg.S(" numerrs: "); Dbg.I(mod.nofErrs); 
   Dbg.Ln;
   FOR i := 0 TO mod.nofErrs-1 DO
      Symtab.Announce(mod.errs[i], par.curFile)
   END;
   qn.module := "";
   qn.name := "x";
   cc := Symtab.FindConst(mod, qn);
   IF cc # NIL THEN
      Dbg.S("X = "); Dbg.I(cc.val.ival); Dbg.Ln
   END;

   ty := mod.types;
   WHILE ty # NIL DO
      Dbg.S(ty.name); Dbg.Ln;
      Ty.DbgPrint(ty.ty, 1);
      Dbg.Ln;
      ty := ty.next
   END
END TestSymtab.
