(* Calculates dependencies for compiling *)
MODULE Deps;
IMPORT
   Ast, BinReader, Config, Cvt:=extConvert, Dbg, Files, 
   Lex:=Scanner, Parser, Path, St:=Symtab, Strings;

CONST
   MaxNameLen=64;
   MaxDeps=512;

   (* flags for ModList.flags *)
   MLNeedsBuild=0;MLProcessed=1;MLNoSource=2;MLRoot=3;

TYPE
   ModList = POINTER TO ModListDesc;
   ModListDesc = RECORD
      name: ARRAY MaxNameLen OF CHAR;
      src: Path.T;
      id: INTEGER;
      flags: SET;
      depth: INTEGER;
         (* Ordered by a walk from the root such that
            if n depends on o directory or indirectly, 
            n.depth < o.depth. Used to partially order
            the dependencies *)
      next: ModList
   END;

   (* A dependency between two modules *)
   Dep = RECORD
      from, to: INTEGER
   END;

   DepState* = RECORD
      mods: ModList;
      numMods: INTEGER;
      deps: ARRAY MaxDeps OF Dep;
      numDeps: INTEGER
   END;

VAR
   AddDepsFwd: PROCEDURE(VAR ds: DepState; scan: Lex.T; ast: Ast.Branch;
                         fileName: ARRAY OF CHAR): BOOLEAN;
   AddDepsForModFwd: PROCEDURE(VAR ds: DepState; 
                               modName: ARRAY OF CHAR): BOOLEAN;

PROCEDURE InitDepState*(VAR ds: DepState); 
BEGIN
   ds.mods := NIL;
   ds.numDeps := 0;
   ds.numMods := 0;
END InitDepState;

PROCEDURE ModFor(ds: DepState; id: INTEGER): ModList;
VAR ml: ModList;
BEGIN
   ml := ds.mods;
   WHILE (ml # NIL) & (ml.id # id) DO
      ml := ml.next
   END;
   RETURN ml
END ModFor;

PROCEDURE Root(ds: DepState): ModList;
VAR ml: ModList;
BEGIN
   ml := ds.mods;
   WHILE (ml # NIL) & ~(MLRoot IN ml.flags) DO
      ml := ml.next
   END;
   RETURN ml
END Root;

PROCEDURE AddDep(VAR ds: DepState; from, to: INTEGER);
BEGIN
   ds.deps[ds.numDeps].from := from;
   ds.deps[ds.numDeps].to := to;
   INC(ds.numDeps)
END AddDep;

PROCEDURE InternMod(VAR ds: DepState; name: ARRAY OF CHAR): ModList;
VAR ml: ModList;
BEGIN
   ml := ds.mods;
   WHILE (ml # NIL) & ~Ast.StringEq(name, ml.name) DO
      ml := ml.next;
   END;
   IF ml = NIL THEN
      NEW(ml);
      ml.depth := 0;
      ml.name := name;
      ml.flags := {};
      IF ds.mods = NIL THEN
         (* assume first file is the root file *)
         ml.flags := {MLRoot}
      END;
      ml.id := ds.numMods; INC(ds.numMods);
      ml.next := ds.mods;
      ds.mods := ml
   END;
   RETURN ml
END InternMod;

PROCEDURE AddSymtabDeps(VAR ds: DepState; modPath: Path.T): BOOLEAN;
   (* Only used for sourceless modules where we only have a 
      symbols file *)
VAR mod, import: St.Module;
    rd: BinReader.T;
    rv: BOOLEAN;
    ml, dml: ModList;
BEGIN
   IF BinReader.Init(rd, modPath.str) THEN
      mod := St.Read(rd);
      BinReader.Finish(rd);
      ml := InternMod(ds, mod.name);
      INCL(ml.flags, MLNoSource);
      import := mod.imports;
      rv := TRUE;
      WHILE rv & (import # NIL) DO
         dml := InternMod(ds, import.name);
         AddDep(ds, ml.id, dml.id);
         rv := AddDepsForModFwd(ds, import.name);
         import := import.importNext
      END
   ELSE
      Dbg.S("ERROR: Could not load symbol file for "); 
      Dbg.S(modPath.str); Dbg.Ln;
      rv := FALSE
  END
  RETURN rv
END AddSymtabDeps;

PROCEDURE AddDepsForMod(VAR ds: DepState; modName: ARRAY OF CHAR): BOOLEAN;
VAR rv: BOOLEAN;
    src: Path.T;
    par: Parser.T;
    ast: Ast.Branch;
BEGIN
   IF Config.FindSrcPath(modName, src) THEN
      par := Parser.NewFromFile(src.str);
      ast := Parser.ParseModule(par, TRUE);
      rv := AddDepsFwd(ds, par.scan, ast, src.str)
   ELSE 
      IF Config.FindSrclessModPath(modName, src) THEN
         rv := AddSymtabDeps(ds, src)
      ELSE
         Dbg.S("ERROR: Could not find source for module ");
         Dbg.S(modName); Dbg.S(".");Dbg.Ln;
         rv := FALSE
      END
   END
   RETURN rv
END AddDepsForMod;

PROCEDURE AddDeps*(VAR ds: DepState; scan: Lex.T; ast: Ast.Branch;
                   fileName: ARRAY OF CHAR): BOOLEAN;
   (* Call with the AST for the root file that is 
      being compiled.  This adds this module, and
      builds a transitive import graph.  No checks
      on file dates is done yet. Prereq: InitDepState.
      Returns FALSE if there was an error. *)
VAR ml, dml: ModList;
    buf: ARRAY 64 OF CHAR;
    nt: Ast.Terminal;
    imports, import: Ast.Branch;
    i: INTEGER;
    rv: BOOLEAN;
BEGIN
   nt := Ast.TermAt(ast, Ast.ModuleName);
   Lex.Extract(scan, nt.tok, buf);
   ml := InternMod(ds, buf);
   Path.FromZ(ml.src, fileName);

   IF ~(MLProcessed IN ml.flags) THEN
      INCL(ml.flags, MLProcessed);
      imports := Ast.BranchAt(ast, Ast.ModuleImports);
      IF imports # NIL THEN
         rv := TRUE;
         i := 0;
         WHILE rv & (i < imports.childLen) DO
            import := Ast.BranchAt(imports, i);
            nt := Ast.TermAt(import, Ast.ImportModule);
            Lex.Extract(scan, nt.tok, buf);
            dml := InternMod(ds, buf);
            AddDep(ds, ml.id, dml.id);
            IF ~(MLProcessed IN dml.flags) THEN
               rv := AddDepsForMod(ds, dml.name)
            END;
            INC(i)
         END
      END
   END
   RETURN rv
END AddDeps;

PROCEDURE Max(a, b: INTEGER): INTEGER;
VAR rv: INTEGER;
BEGIN
   IF a > b THEN
      rv := a
   ELSE
      rv := b
   END
   RETURN rv
END Max;

PROCEDURE CheckNeedsBuild(ds: DepState; ml: ModList): BOOLEAN;
   (* Propagates any NeedsBuild flags down through
      the tree towards the root.  Also set's the depth 
      field used for ordering while doing the walk. *)
VAR i:INTEGER;
    chld: ModList;
    anyDirty: BOOLEAN;
BEGIN
   anyDirty := MLNeedsBuild IN ml.flags;
   FOR i := 0 TO ds.numDeps-1 DO
      IF ds.deps[i].from = ml.id THEN
         chld := ModFor(ds, ds.deps[i].to);
         chld.depth := Max(chld.depth, ml.depth+1);
         anyDirty := CheckNeedsBuild(ds, chld) OR anyDirty
      END
   END;
   IF anyDirty THEN
      INCL(ml.flags, MLNeedsBuild)
   END;
   RETURN anyDirty
END CheckNeedsBuild;

PROCEDURE GetModTime(p: Path.T; VAR time, date: INTEGER);
VAR fh: Files.File;
BEGIN
   time := 0; date := 0;
   fh := Files.Old(p.str);
   IF fh # NIL THEN
      Files.GetDate(fh, time, date);
      Files.Close(fh)
   END
END GetModTime;
  
PROCEDURE SrcNewerThanSyms(ml: ModList): BOOLEAN;
VAR rv: BOOLEAN;
    modpath: Path.T;
    srcT, srcD, modT, modD: INTEGER;
BEGIN
   IF Config.FindModPath(ml.name, modpath) THEN
      GetModTime(ml.src, srcT, srcD);
      GetModTime(modpath, modT, modD);
      rv := (srcD > modD) OR ((srcD = modD) & (srcT > modT))  
   ELSE
      rv := TRUE
   END
   RETURN rv
END SrcNewerThanSyms;

PROCEDURE Check*(VAR ds: DepState): BOOLEAN; 
   (* Marks modules that need to be rebuilt, by 
      checking for changes, and propagating the
      NeedsBuild flag down back towards the root *)
VAR ml: ModList;
BEGIN
   ml := ds.mods;
   WHILE ml # NIL DO
      IF ~(MLNoSource IN ml.flags) & ~(MLNeedsBuild IN ml.flags) THEN
         IF SrcNewerThanSyms(ml) THEN
            INCL(ml.flags, MLNeedsBuild)
         END;
      END;
      ml := ml.next
   END;
   RETURN CheckNeedsBuild(ds, Root(ds))
END Check;

PROCEDURE NextDirtyFile*(ds: DepState; VAR dest: Path.T): BOOLEAN;
   (* Prereqs: AddDeps, Check.  Returns the next file that should
      be compiled, or FALSE if there are none left *)
VAR rv: BOOLEAN;
    ml, cand: ModList;
    largestDepth: INTEGER;
BEGIN
   largestDepth := -1;
   ml := ds.mods;
   cand := NIL;
   WHILE ml # NIL DO
      IF (MLNeedsBuild IN ml.flags) &  (ml.depth > largestDepth) THEN
         largestDepth := ml.depth;
         cand := ml
      END;
      ml := ml.next
   END;
   IF cand # NIL THEN
      rv := TRUE;
      EXCL(cand.flags, MLNeedsBuild);
      Path.Copy(cand.src, dest)
   ELSE
      rv := FALSE
   END
   RETURN rv
END NextDirtyFile;

(* Files seems more geared towards serialization, 
   so there's no way to write a string in one call
   without also writing a terminating byte. So we
   also do this weird position shifting *)
PROCEDURE OS(fh: Files.File; VAR rd: Files.Rider; txt: ARRAY OF CHAR);
BEGIN
   IF Strings.Length(txt) > 0 THEN
      Files.WriteString(rd, txt);
      Files.Set(rd, fh, Files.Pos(rd) - 1)
   END
END OS;

PROCEDURE OLn(VAR rd: Files.Rider);
VAR bb: ARRAY 1 OF BYTE;
BEGIN
   bb[0] := 10;
   Files.WriteBytes(rd, bb, 1);
END OLn;

PROCEDURE WriteGraphViz*(ds: DepState; fname: ARRAY OF CHAR);
   (* Writes a graphviz file for visualizing the dependencies *)
VAR fh: Files.File;
    rd: Files.Rider;
    i: INTEGER;
    ml: ModList;
    pbuf: ARRAY 40 OF CHAR;
    ignore: BOOLEAN;
BEGIN
   fh := Files.New(fname);
   IF fh # NIL THEN
      Files.Set(rd, fh, 0);
      OS(fh, rd, "digraph {"); OLn(rd);
      FOR i := 0 TO ds.numDeps-1 DO
         ml := ModFor(ds, ds.deps[i].from);
         OS(fh, rd, ml.name); 
         Cvt.IntToString(ml.depth, pbuf, ignore);
         OS(fh, rd, pbuf);
         OS(fh, rd, " -> ");
         ml := ModFor(ds, ds.deps[i].to);
         OS(fh, rd, ml.name); 
         Cvt.IntToString(ml.depth, pbuf, ignore);
         OS(fh, rd, pbuf);
         OS(fh, rd, ";");
         OLn(rd)
      END; 
      OS(fh, rd, "}");
      OLn(rd);
      Files.Register(fh);
      Files.Close(fh)
   END
END WriteGraphViz;

BEGIN
   AddDepsFwd := AddDeps;
   AddDepsForModFwd := AddDepsForMod
END Deps.
