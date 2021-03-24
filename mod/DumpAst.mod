(* Command for debugging parsing *)
MODULE DumpAst;
IMPORT Ast, Dbg, Par:=Parser, Render, Semcheck, 
       Symtab, Args:=extArgs;

PROCEDURE DoIt();
VAR par: Par.T;
    root: Ast.T;
    fname: ARRAY 1024 OF CHAR;
    i, j, res: INTEGER;
    semcheck, render: BOOLEAN;
    scstate: Semcheck.State;
    mod: Symtab.Module;
BEGIN
   semcheck := FALSE;
   FOR i := 0 TO Args.count-1 DO
      Args.Get(i, fname, res);
      ASSERT(res = 0);
      IF Ast.StringEq(fname, "-y") THEN
         semcheck := TRUE
      ELSIF Ast.StringEq(fname, "-r") THEN
         render := TRUE
      ELSE
         Dbg.S("FILE: "); Dbg.S(fname); Dbg.Ln;
         par := Par.NewFromFile(fname);
         root := Par.ParseModule(par, FALSE);
         IF semcheck THEN 
            mod := Symtab.BuildModule(root, par.scan);
            Semcheck.Init(scstate, mod, par.scan);
            IF ~Semcheck.Run(scstate) THEN 
               Dbg.S("   Warning: semcheck failed."); Dbg.Ln;
               FOR j := 0 TO mod.nofErrs-1 DO
                  Ast.Announce(mod.errs[j], par.scan, fname)
               END;
            END;
         END;
         root.ops.toStr(root, par.scan.buf, 0);
         IF render THEN
            Render.Init("/tmp/rendered.mod", par.scan);
            Render.WriteTree(root);
            Render.Deinit();
         END;
         Dbg.Ln;   
      END
   END
END DoIt;

BEGIN
   DoIt
END DumpAst.
