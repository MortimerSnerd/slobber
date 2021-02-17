(* Configuration that controls directory locations and behavior *)
MODULE Config;
IMPORT 
   Env:=extEnv, Files, Path;

TYPE
   T = RECORD
      srcPaths: ARRAY 16 OF Path.T;
      modPaths: ARRAY 8 OF Path.T;
      sysPath, modOutPath: Path.T;
      numSrcPaths, numModPaths: INTEGER
   END;
VAR cfg: T;

(* Hacky way to get the executable path.  If this goes wrong, 
   it doesn't look to hard to add modules written in C to 
   OBNC. *)
PROCEDURE GetExePath(VAR p: Path.T); 
VAR fh: Files.File;
    rd: Files.Rider;
    b: BYTE;
    i: INTEGER;
BEGIN
   fh := Files.Old("/proc/self/maps");
   Files.Set(rd, fh, 0);
   (* Skip to first path on first line *)
   REPEAT
      Files.Read(rd, b)
   UNTIL (b = 0) OR (b = ORD("/"));
   IF b # 0 THEN
      i := 0;
      REPEAT
         p.str[i] := CHR(b);
         INC(i);
         Files.Read(rd, b)
      UNTIL b = 10;
      p.len := i;
      p.str[p.len] := 0X
   END;
   Files.Close(fh)
END GetExePath;
      


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

PROCEDURE PathSearch(modName: ARRAY OF CHAR; 
                     ext: ARRAY OF CHAR;
                     locations: ARRAY OF Path.T; 
                     numLocs: INTEGER;
                     VAR path: Path.T): BOOLEAN;
   (* Searchs for modName"."ext in the given locations. 
      Returns the "path" and TRUE for the first one found. *)
VAR rv: BOOLEAN;
    i: INTEGER;
BEGIN
   i := 0;
   rv := FALSE;
   WHILE ~rv & (i < numLocs) DO
      IF locations[i].len > 0 THEN 
         Path.Copy(locations[i], path);
         Path.Append(path, modName);
         Path.Append(path, ext);
         IF Exists(path) THEN
            rv := TRUE
         END
      END;
      INC(i)
   END;
   RETURN rv
END PathSearch;

PROCEDURE FindModPath*(modName: ARRAY OF CHAR; VAR path: Path.T): BOOLEAN;
   RETURN PathSearch(modName, ".slo", cfg.modPaths, cfg.numModPaths, path)
END FindModPath;

PROCEDURE FindSrclessModPath*(modName: ARRAY OF CHAR; 
                              VAR path: Path.T): BOOLEAN;
   (* Looks for symbol file for one of the external modules
      where we only have the symbol file *)
BEGIN
   Path.Copy(cfg.sysPath, path);
   Path.Append(path, modName);
   Path.Append(path, ".slo");
   RETURN Exists(path)
END FindSrclessModPath;

PROCEDURE FindSrcPath*(modName: ARRAY OF CHAR; VAR path: Path.T): BOOLEAN;
   RETURN PathSearch(modName, ".mod", cfg.srcPaths, cfg.numSrcPaths, path)
END FindSrcPath;         

PROCEDURE AddSrcPath*(p: Path.T);
   (* Adds a path to the source file search paths *)
BEGIN
   ASSERT(cfg.numSrcPaths < LEN(cfg.srcPaths));
   Path.Copy(p, cfg.srcPaths[cfg.numSrcPaths]);
   Path.AssertDir(cfg.srcPaths[cfg.numSrcPaths]);
   INC(cfg.numSrcPaths)
END AddSrcPath; 

PROCEDURE Init();
VAR i, res: INTEGER;
BEGIN
   FOR i := 0 TO LEN(cfg.srcPaths)-1 DO
      Path.Zero(cfg.srcPaths[i])
   END;
   FOR i := 0 TO LEN(cfg.modPaths)-1 DO
      Path.Zero(cfg.modPaths[i])
   END;
   (* Base sys config dirs from exe location.  exeloc/../lib *)
   cfg.numSrcPaths := 1;
   cfg.numModPaths := 2;
   GetExePath(cfg.sysPath);
   Path.Drop(cfg.sysPath); Path.Drop(cfg.sysPath);
   Path.Append(cfg.sysPath, "lib/");
   Env.Get("PWD", cfg.srcPaths[0].str, res); 
   ASSERT(res = 0);
   Path.Update(cfg.srcPaths[0]);
   Path.AssertDir(cfg.srcPaths[0]);
   Path.Copy(cfg.srcPaths[0], cfg.modOutPath);
   Path.Copy(cfg.modOutPath, cfg.modPaths[0]);
   (* Possibly temporary hack for external modules we have a 
      symbol file for, but no source.  So the sourceless library
      modules for OBNC *)
   Path.Copy(cfg.sysPath, cfg.modPaths[1]);
END Init;

BEGIN
   Init()
END Config.
