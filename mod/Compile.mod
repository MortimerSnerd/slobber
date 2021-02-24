(* Command line driver *)
MODULE Compile;
IMPORT
   AMD64, Args:=extArgs, Config, Compiler, Dbg, Path,
   Target;

PROCEDURE Usage;
BEGIN
   Dbg.S("Compile [OPTS] rootSrcFile"); Dbg.Ln;
   Dbg.S("   -y       - generate symbol files only"); Dbg.Ln;
   Dbg.S("   -s path  - Add path to search path for source files"); Dbg.Ln;
END Usage;

PROCEDURE Go();
VAR opts: Compiler.Options;
    path, arg: Path.T;
    res: INTEGER;
    i: INTEGER;
    good: BOOLEAN;
BEGIN
   Path.Zero(path);
   Compiler.InitOptions(opts);
   good := TRUE;
   i := 0;
   WHILE good & (i < Args.count) DO
      Args.Get(i, arg.str, res);
      Path.Update(arg);
      ASSERT(res = 0);
      IF arg.str[0] = "-" THEN
         IF arg.str[1] = "y" THEN
            INCL(opts.flags, Compiler.OnlyGenSyms)
         ELSIF arg.str[1] = "s" THEN
            INC(i);
            IF i < Args.count THEN
               Args.Get(i, arg.str, res); Path.Update(arg);
               ASSERT(res = 0);
               Config.AddSrcPath(arg)
            ELSE
               Dbg.S("ERROR: Expecting path after final option");
               Dbg.Ln; good := FALSE
            END
         ELSE
            Dbg.S("ERROR: Unrecognized option: "); 
            Dbg.S(arg.str); Dbg.Ln; 
            good := FALSE
         END
      ELSE
         IF path.len = 0 THEN
            Path.Copy(arg, path)
         ELSE
            Dbg.S("ERROR: Only specify one root source file.");
            Dbg.Ln;
            good := FALSE
         END
      END;
      INC(i)
   END;
   IF path.len = 0 THEN
      Dbg.S("ERROR: No source file specified."); Dbg.Ln;
      good := FALSE;
   END; 
   (* hardcoding *)
   opts.targ := Target.MkBlankTarget();
   AMD64.Init(opts.targ);

   IF good THEN
      IF Compiler.Compile(path.str, opts) THEN
         Dbg.S("Good")
      ELSE
         Dbg.S("Bad")
      END;
      Dbg.Ln
   ELSE
      Usage
   END
END Go;

BEGIN
   Go
END Compile.
