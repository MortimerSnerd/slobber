(* Configuration that controls directory locations and behavior *)
MODULE Config;
IMPORT 
   Env:=extEnv, Files, Path;

TYPE
   T = RECORD
      srcPaths: ARRAY 16 OF Path.T;
      modPaths: ARRAY 8 OF Path.T;
      sysPath, modOutPath: Path.T
   END;
VAR cfg: T;

(* Given a module name, returns the path to write the 
   symbtable to *)
PROCEDURE ModOutFile*(modName: ARRAY OF CHAR; VAR dest: Path.T);
BEGIN
   Path.Copy(cfg.modOutPath, dest);
   Path.Append(dest, modName);
   Path.Append(dest, ".slo")
END ModOutFile;

(* Returns the path for the builtins symtab file, which is 
   hardcoded and out of the normal path, so its name will
   never conflict with another module *)
PROCEDURE BuiltinsSymtabFile*(VAR path: Path.T);
BEGIN
   Path.Copy(cfg.sysPath, path);
   Path.Append(path, "stubs/Globals.slo")
END BuiltinsSymtabFile;

PROCEDURE Exists(p: Path.T): BOOLEAN;
VAR fh: Files.File;
    rv: BOOLEAN;
BEGIN
   fh := Files.Old(p.str);
   IF fh # NIL THEN
      rv := TRUE;
      Files.Close(fh)
   ELSE
      rv := FALSE
   END;
   RETURN rv
END Exists;

PROCEDURE FindModPath*(modName: ARRAY OF CHAR; VAR path: Path.T): BOOLEAN;
VAR rv: BOOLEAN;
    i: INTEGER;
BEGIN
   i := 0;
   rv := FALSE;
   WHILE ~rv & (i < LEN(cfg.modPaths)) DO
      IF cfg.modPaths[i].len > 0 THEN 
         Path.Copy(cfg.modPaths[i], path);
         Path.Append(path, modName);
         Path.Append(path, ".slo");
         IF Exists(path) THEN
            rv := TRUE
         END
      END;
      INC(i)
   END;
   RETURN rv
END FindModPath;

PROCEDURE Init();
VAR i, res: INTEGER;
BEGIN
   FOR i := 0 TO LEN(cfg.srcPaths)-1 DO
      Path.Zero(cfg.srcPaths[i])
   END;
   FOR i := 0 TO LEN(cfg.modPaths)-1 DO
      Path.Zero(cfg.modPaths[i])
   END;
   (* TODO - get exe dir from /proc/self and then base sysPath on that *)
   Path.FromZ(cfg.sysPath, "./");
   Env.Get("PWD", cfg.srcPaths[0].str, res); 
   ASSERT(res = 0);
   Path.Update(cfg.srcPaths[0]);
   Path.AssertDir(cfg.srcPaths[0]);
   Path.Copy(cfg.srcPaths[0], cfg.modOutPath);
   Path.Copy(cfg.modOutPath, cfg.modPaths[0]);
END Init;

BEGIN
   Init()
END Config.
