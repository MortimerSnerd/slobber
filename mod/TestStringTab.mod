MODULE TestStringTab;
IMPORT Dbg, extArgs, Files, Strings, StringTab;

PROCEDURE RunFile(path: ARRAY OF CHAR);
VAR ride: Files.Rider;
    fh: Files.File;
    buf, rdback: ARRAY 1024 OF CHAR;
    sym: StringTab.SymPtr;
    tab: StringTab.TablePtr;
    i: INTEGER;
BEGIN
   tab := StringTab.MkTable();
   fh := Files.Old(path);
   ASSERT(fh # NIL);
   Files.Set(ride, fh, 0);
   WHILE ~ride.eof DO
      Files.ReadString(ride, buf);
      sym := StringTab.SymFor(tab, buf);
      ASSERT(sym # NIL);
      StringTab.Extract(tab, sym, rdback);
      ASSERT(Strings.Length(buf) = Strings.Length(rdback));
      FOR i := 0 TO sym.len DO
         ASSERT(buf[i] = rdback[i])
      END;
   END;
   sym := StringTab.FirstSym(tab);
   WHILE sym # NIL DO
      StringTab.Extract(tab, sym, buf);
      Dbg.S(buf); Dbg.Ln;
      sym := StringTab.NextSym(sym)
   END;
   Files.Close(fh);
END RunFile;

PROCEDURE Go;
VAR path: ARRAY 1024 OF CHAR;
    res: INTEGER;
BEGIN
   IF extArgs.count = 1 THEN
      extArgs.Get(0, path, res);
      ASSERT(res = 0);
      RunFile(path); 
   ELSE
      Dbg.S("Usage: TestStringTab testfile")
   END
END Go;

BEGIN
   Go;
END TestStringTab.
