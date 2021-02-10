MODULE TestSymtab;
IMPORT
   Ast, BinReader, BinWriter, Dbg, In, Par:=Parser, 
   Semcheck, Symtab, Ty:=Types;
VAR
   DbgPrintFrame: PROCEDURE(frame: Symtab.Frame; indent: INTEGER; 
                            showProcFrame: BOOLEAN);

PROCEDURE DbgPrintTypeSym(ts: Symtab.TypeSym; indent: INTEGER; 
                          showScope: BOOLEAN);
BEGIN
   WHILE ts # NIL DO
      Dbg.Ind(indent); Dbg.S(ts.name); 
      IF ts.export THEN Dbg.S("*") END;
      Dbg.Ln;
      Ty.DbgPrint(ts.ty, indent+1);
      Dbg.Ln;
      IF (ts.frame # NIL) & showScope THEN
         Dbg.Ind(indent); Dbg.S("SCOPE FRAME"); Dbg.Ln;
         DbgPrintFrame(ts.frame, indent+2, showScope);
         Dbg.Ln; Dbg.Ln
      END;
      ts := ts.next
   END;
   Dbg.Ln
END DbgPrintTypeSym;

PROCEDURE DbgPrintFrameImpl(frame: Symtab.Frame; indent: INTEGER; 
                        showProcFrame: BOOLEAN);
   PROCEDURE Section(heading: ARRAY OF CHAR; syms: Symtab.TypeSym; 
                     indent: INTEGER; showProcFrame: BOOLEAN);
   BEGIN
      IF syms # NIL THEN
         Dbg.Ln; Dbg.Ind(indent);  Dbg.S(heading);  Dbg.Ln;
         DbgPrintTypeSym(syms, indent+1, showProcFrame);
      END;
   END Section;
BEGIN
   (* TODO consts *)
   Dbg.Ln;Dbg.Ind(indent);Dbg.S("FRAME");
   IF Symtab.ffProcParams IN frame.flags THEN Dbg.S(" ffProcParams") END;
   IF Symtab.ffModule IN frame.flags THEN Dbg.S(" ffModule") END;
   Section("*** TYPES", frame.types, indent, FALSE);
   Section("*** VARS", frame.vars, indent, FALSE);
   Section("*** PROCEDURES", frame.procedures, indent, TRUE);
END DbgPrintFrameImpl;

PROCEDURE CheckTypeWriting(mod: Symtab.Module);
VAR ts: Symtab.TypeSym;
    rd: BinReader.T;
    wr: BinWriter.T;
    nty: Ty.Type;
    fname: ARRAY 128 OF CHAR;
BEGIN
   fname := "/tmp/tytest.bin";
   ts := mod.frame.types;
   WHILE ts # NIL DO
      IF BinWriter.Init(wr, fname) THEN
         Ty.Write(wr, ts.ty);
         BinWriter.Finish(wr);
         IF BinReader.Init(rd, fname) THEN
            nty := Ty.Read(rd);
            BinReader.Finish(rd);
            IF ~Ty.Equal(ts.ty, nty) THEN
               Dbg.S("error: type not equal for ");
               Dbg.S(ts.name); Dbg.Ln
            END;
         ELSE
            Dbg.Ln;
            Dbg.S("Error: could not open type file for reading");
            Dbg.Ln
         END
      ELSE
         Dbg.Ln;
         Dbg.S("Error: could not open type file for reading");
         Dbg.Ln
      END;
      ts := ts.next
   END
END CheckTypeWriting;

PROCEDURE TestFiles();
VAR fname: ARRAY 1024 OF CHAR;
   par: Par.T;
   ast: Ast.T;
   mod: Symtab.Module;
   scstate: Semcheck.State;
   i: INTEGER;
BEGIN
   REPEAT
      In.Line(fname);
      IF fname[0] # 0X THEN
         Dbg.S("Checking "); Dbg.S(fname);Dbg.S("..."); 
         par := Par.NewFromFile(fname);
         ast := Par.ParseModule(par);
         mod := Symtab.BuildModule(ast, par.scan);
         CheckTypeWriting(mod);
         Semcheck.Init(scstate, mod, par.scan);
         IF Semcheck.Run(scstate) THEN
            Dbg.S("Done")
         ELSE
            Dbg.S("Failed");
         END;
         Dbg.Ln;
         FOR i := 0 TO mod.nofErrs-1 DO
            Ast.Announce(mod.errs[i], par.curFile)
         END
      END
   UNTIL fname[0] = 0X;
END TestFiles;
   

BEGIN
   DbgPrintFrame := DbgPrintFrameImpl;
   TestFiles;
END TestSymtab.
