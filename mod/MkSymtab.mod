(* Tool to generate symtab files for modules without 
   compiling the module. Does not follow imports
   to recursively build tables for dependencies, 
   so caller will need to order calls to start a
   the leaves and then work down the tree from there*)
MODULE MkSymtab;
IMPORT
   Args:=extArgs, Ast, BinWriter, Config, Dbg, Par:=Parser, 
   Path, Semcheck, Symtab; 

PROCEDURE Build();
VAR srcFile, modName, outFile: Path.T;
   par: Par.T;
   ast: Ast.T;
   mod: Symtab.Module;
   scstate: Semcheck.State;
   i, j, res: INTEGER;
   wr: BinWriter.T;
BEGIN
   Dbg.S("Converting modules.");Dbg.Ln;
   FOR i := 0 TO Args.count-1 DO
      Args.Get(i, srcFile.str, res);
      ASSERT(res = 0);
      Path.Update(srcFile);
      Path.Basename(srcFile, modName);
      Path.StripExt(modName);
      Config.ModOutFile(modName.str, outFile);
      Dbg.S("   "); Dbg.S(srcFile.str); 
      Dbg.S(" -> "); Dbg.S(outFile.str);
      Dbg.S("...");
      par := Par.NewFromFile(srcFile.str);
      ast := Par.ParseModule(par, FALSE);
      (* ast.ops.toStr(ast, par.scan.buf, 0); *)
      mod := Symtab.BuildModule(ast, par.scan);
      Semcheck.Init(scstate, mod, par.scan);
      IF Semcheck.Run(scstate) THEN
         IF mod.nofErrs = 0 THEN
            IF BinWriter.Init(wr, outFile.str) THEN
               Symtab.Write(wr, mod);
               BinWriter.Finish(wr);
               Dbg.S("Done");
            ELSE
               Dbg.S("Failed write")
            END
         ELSE
            Dbg.S("Failed symcheck") 
         END
      ELSE
         Dbg.S("Failed");
      END;
      (* Dbg.Ln;Dbg.S("transformed");Dbg.Ln;
      ast.ops.toStr(ast, par.scan.buf, 0);
      Dbg.Ln; *)

      Dbg.Ln;
      FOR j := 0 TO mod.nofErrs-1 DO
         Ast.Announce(mod.errs[j], par.scan, par.curFile)
      END;
   END
END Build;

BEGIN
   Build()
END MkSymtab.
