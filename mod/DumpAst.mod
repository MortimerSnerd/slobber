(* Command for debugging parsing *)
MODULE DumpAst;
IMPORT Ast, Dbg, Par:=Parser, Lex:=Scanner, Semcheck, 
       Symtab, Args:=extArgs;

PROCEDURE DoIt();
VAR par: Par.T;
    root: Ast.T;
    pos: Ast.SourcePos;
    fname: ARRAY 1024 OF CHAR;
    i, res: INTEGER;
    semcheck: BOOLEAN;
    scstate: Semcheck.State;
    mod: Symtab.Module;
BEGIN
   semcheck := FALSE;
   FOR i := 0 TO Args.count-1 DO
      Args.Get(i, fname, res);
      ASSERT(res = 0);
      IF Ast.StringEq(fname, "-y") THEN
         semcheck := TRUE
      ELSE
         Dbg.S("FILE: "); Dbg.S(fname); Dbg.Ln;
         par := Par.NewFromFile(fname);
         root := Par.ParseModule(par, FALSE);
         IF semcheck THEN 
            mod := Symtab.BuildModule(root, par.scan);
            Semcheck.Init(scstate, mod, par.scan);
            IF ~Semcheck.Run(scstate) THEN 
               Dbg.S("   Warning: semcheck failed."); Dbg.Ln;
            END;
         END;
         root.ops.toStr(root, par.scan.buf, 0);
         Dbg.Ln;   
      END
   END
END DoIt;

BEGIN
   DoIt
END DumpAst.
