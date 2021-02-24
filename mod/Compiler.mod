(* Driver for compilation *)
MODULE Compiler;
IMPORT
   Ast, BinWriter, Config, Dbg, Deps, Il, Parser, Path, 
   Semcheck, St:=Symtab, Target;

CONST
   (* Option flags *)
   OnlyGenSyms*=0;

TYPE
   Options* = RECORD 
      flags*: SET;
      targ*: Target.T
   END;

PROCEDURE InitOptions*(VAR op: Options);
BEGIN
   op.flags := {}
END InitOptions;

PROCEDURE CompileFile(path: Path.T; isMain: BOOLEAN;
                      opts: Options): BOOLEAN;
VAR ast: Ast.Branch;
    par: Parser.T;   
    mod: St.Module;
    rv: BOOLEAN;
    scstate: Semcheck.State;
    wr: BinWriter.T;
    outFile: Path.T;
    i: INTEGER;
    gs: Il.GenerateState;
BEGIN
   rv := TRUE;
   par := Parser.NewFromFile(path.str);
   ast := Parser.ParseModule(par, FALSE);
   IF ast # NIL THEN
      mod := St.BuildModule(ast, par.scan);   
      Semcheck.Init(scstate, mod, par.scan);
      IF (mod.nofErrs = 0) & Semcheck.Run(scstate) THEN
         IF isMain & ~(OnlyGenSyms IN opts.flags) THEN
            (* do some actual compilation and linking here *)
         ELSE
            Config.ModOutFile(mod.name, outFile);
            IF BinWriter.Init(wr, outFile.str) THEN
               St.Write(wr, mod);
               BinWriter.Finish(wr);
               (* TODO technically need to also call something here
                  to create .o files or anything we can actually link *)
            ELSE
               Dbg.S("ERROR: could not write "); 
               Dbg.S(outFile.str); Dbg.Ln;
               rv := FALSE
            END
         END;
         IF rv & ~(OnlyGenSyms IN opts.flags) THEN
            Il.InitGenerateState(gs, mod, par.scan, opts.targ);
            Il.Generate(gs);
            Il.LogCode(gs);
            (*TODOOD*)
         END
      ELSE
         FOR i := 0 TO mod.nofErrs-1 DO
            Ast.Announce(mod.errs[i], par.scan, path.str)
         END;
         rv := FALSE
      END;
   ELSE
      rv := FALSE
   END
   RETURN rv
END CompileFile;

PROCEDURE Compile*(rootSrcFile: ARRAY OF CHAR; opts: Options): BOOLEAN;
VAR ast: Ast.Branch;
    par: Parser.T;
    deps: Deps.DepState;
    rv: BOOLEAN;
    depPath: Path.T;
BEGIN
   rv := TRUE;
   par := Parser.NewFromFile(rootSrcFile);
   ast := Parser.ParseModule(par, TRUE);
   IF ast # NIL THEN
      Deps.InitDepState(deps);
      IF Deps.AddDeps(deps, par.scan, ast, rootSrcFile) THEN
         IF Deps.Check(deps) THEN
            Deps.WriteGraphViz(deps, "/tmp/balls.gv");
            WHILE rv & Deps.NextDirtyFile(deps, depPath) DO
               Dbg.S("   Compiling "); Dbg.S(depPath.str); Dbg.Ln;
               rv := CompileFile(depPath, Ast.StringEq(rootSrcFile, depPath.str), 
                                 opts);
            END
         END
      ELSE
         rv := FALSE
      END 
   ELSE
      rv := FALSE
   END;
   
   RETURN rv
END Compile;


END Compiler.
