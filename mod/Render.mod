(* Test module for turning a AST tree back into source *)
MODULE Render;
IMPORT
   Ast, Dbg, Files, Lex := Scanner;

CONST
   Indent=3;

TYPE
   State* = RECORD
      fh: Files.File;
      rd: Files.Rider;
      scan: Lex.T;
      indent: INTEGER
   END;

VAR
   st: State;
   indentString: ARRAY Indent+1 OF CHAR;
   WriteTree*: PROCEDURE(t: Ast.T);

(* Files seems more geared towards serialization, 
   so there's no way to write a string in one call
   without also writing a terminating byte. So we
   also do this weird position shifting *)
PROCEDURE OS(txt: ARRAY OF CHAR);
BEGIN
   IF LEN(txt) > 1 THEN
      Files.WriteString(st.rd, txt);
      Files.Set(st.rd, st.fh, Files.Pos(st.rd) - 1)
   END
END OS;

PROCEDURE OInd();
VAR i: INTEGER;
BEGIN
   FOR i := 0 TO (st.indent)-1 DO
      OS(indentString)
   END
END OInd;

PROCEDURE OLn;
VAR bb: ARRAY 1 OF BYTE;
BEGIN
   bb[0] := 10;
   Files.WriteBytes(st.rd, bb, 1);
   OInd
END OLn;

PROCEDURE OTk(t: Lex.Token);
VAR buf: ARRAY 255 OF CHAR;
    i: INTEGER;
BEGIN
   FOR i := 0 TO t.len-1 DO
      buf[i] := st.scan.buf[i+t.start]
   END;
   buf[t.len] := 0X;
   OS(buf)
END OTk;
      
PROCEDURE OSemi;
VAR bb: ARRAY 2 OF CHAR;
BEGIN
   bb[0] := ";";
   bb[1] := 0X;
   OS(bb)
END OSemi;
   
PROCEDURE Init*(outfile: ARRAY OF CHAR; scan: Lex.T);
VAR i: INTEGER;
BEGIN
   st.indent := 0;
   st.fh := Files.New(outfile);
   Files.Set(st.rd, st.fh, 0);
   st.scan := scan;
   FOR i := 0 TO Indent-1 DO
      indentString[i] := " "
   END;
   indentString[Indent] := 0X
END Init;

PROCEDURE Deinit*();
   VAR fp: INTEGER;
       bb: ARRAY 1 OF BYTE;
BEGIN
   IF Files.Pos(st.rd) > 0 THEN
      (* Replace trailing null if any *)
      fp := Files.Pos(st.rd);
      bb[0] := 1;
      Files.ReadBytes(st.rd, bb, 1);
      IF bb[0] = 0 THEN
         Files.Set(st.rd, st.fh, fp);
         bb[0] := 32;
         Files.WriteBytes(st.rd, bb, 1)
      END
   END;

   Files.Register(st.fh);
   Files.Close(st.fh)
END Deinit;

PROCEDURE C(b: Ast.Branch; i: INTEGER): Ast.T;
   RETURN Ast.GetChild(b, i)
END C;

PROCEDURE WriteDelimList(b: Ast.Branch; delim: ARRAY OF CHAR;
                         start, stop: INTEGER);
VAR i: INTEGER;
BEGIN
   FOR i := start TO stop DO
      WriteTree(C(b, i));
      IF i < stop THEN OS(delim) END
   END
END WriteDelimList;

(* Writes the terminal without any export sigil *)
PROCEDURE WriteTerminal(n: Ast.T);
BEGIN
   IF n IS Ast.Terminal THEN
      OTk(n(Ast.Terminal).tok)
   ELSE
      Dbg.S("WriteTerminal: not a terminal"); Dbg.Ln;
      ASSERT(FALSE)
   END
END WriteTerminal;

(* Renders any node of a Ast out as source, to whatever 
   file was initialized with Init(). *)
PROCEDURE WriteTreeImpl*(n: Ast.T);
VAR term: Ast.Terminal;
    b, x: Ast.Branch;
    any0: Ast.T;
    i: INTEGER;
BEGIN
   IF n # NIL THEN
      IF n IS Ast.Terminal THEN
         term := n(Ast.Terminal);
         OTk(term.tok);
         IF term.export THEN
            OS("*")
         END
      ELSE
         b := n(Ast.Branch);
         Dbg.S("Branch "); Dbg.S(Ast.BranchNames[b.kind]); Dbg.Ln;
         CASE b.kind OF
            Ast.BkDeclarationSequence, Ast.BkDesignator, Ast.BkProcDeclSeq,
            Ast.BkUnOp:
               FOR i := 0 TO b.childLen-1 DO
                  WriteTreeImpl(C(b, i))
               END

            |Ast.BkBinOp:
               WriteTreeImpl(C(b, 0));
               OS(" ");
               WriteTreeImpl(C(b, 1));
               OS(" ");
               WriteTreeImpl(C(b, 2))

            |Ast.BkConstDeclSeq:
               OS("CONST");
               INC(st.indent); OLn;
               FOR i := 0 TO b.childLen-1 DO
                  WriteTreeImpl(C(b, i))
               END;
               DEC(st.indent);OLn

            |Ast.BkTypeDeclaration:
               WriteTreeImpl(C(b, Ast.TypeDeclName));
               OS(" = ");
               WriteTreeImpl(C(b, Ast.TypeDeclVal))

            |Ast.BkRecordType:
               OS("RECORD");
               any0 := C(b, Ast.RecordBaseType);
               IF any0 # NIL THEN
                  OS("("); WriteTreeImpl(any0); OS(")")
               END;
               INC(st.indent);
               WriteTreeImpl(C(b, Ast.RecordFieldList));
               DEC(st.indent);OLn;
               OS("END")
               
            |Ast.BkFieldList:
               OLn;
               WriteTreeImpl(C(b, Ast.FieldListIdents));
               OS(": ");
               WriteTreeImpl(C(b, Ast.FieldListType))

            |Ast.BkQualIdent:
               any0 := C(b, Ast.QualIdentModule);
               IF any0 # NIL THEN
                  WriteTreeImpl(any0); OS(".")
               END;
               WriteTreeImpl(C(b, Ast.QualIdentName))

            |Ast.BkFieldListSequence:
               FOR i := 0 TO b.childLen-1 DO
                  WriteTreeImpl(C(b, i));
                  IF i < (b.childLen-1) THEN OSemi END
               END

            |Ast.BkVarDeclaration:
               WriteTreeImpl(C(b, Ast.VarDeclIdents));
               OS(": ");
               WriteTreeImpl(C(b, Ast.VarDeclVal))

            |Ast.BkProcedureType:
               OS("PROCEDURE");
               WriteTreeImpl(C(b, 0))

            |Ast.BkFormalParameters:
               any0 := C(b, Ast.FormalParamsReturn);
               IF (b.childLen > 1) OR (any0 # NIL) THEN
                  OS("(");
                  WriteDelimList(b, "; ", Ast.FormalParamsStart, b.childLen-1);
                  OS(")")
               END;
               IF any0 # NIL THEN
                  OS(": ");
                  WriteTreeImpl(any0)
               END

            |Ast.BkFPSection:
               IF Ast.NfVar IN b.flags THEN
                  OS("VAR ")
               END;
               WriteDelimList(b, ", ", 0, b.childLen-2);
               OS(": ");
               WriteTreeImpl(C(b, b.childLen-1))

            |Ast.BkFormalType:
               FOR i := 0 TO b.n-1 DO
                  OS("ARRAY OF ")
               END;
               WriteTreeImpl(C(b, 0))


            |Ast.BkTypeDeclSeq:
               OS("TYPE");
               INC(st.indent); OLn;
               FOR i := 0 TO b.childLen-1 DO
                  WriteTreeImpl(C(b, i));OSemi;OLn; OLn
               END;
               DEC(st.indent); OLn

            |Ast.BkVarDeclSeq:
               OS("VAR");
               INC(st.indent);
               FOR i := 0 TO b.childLen-1 DO
                  OLn; WriteTreeImpl(C(b, i)); OSemi
               END;
               DEC(st.indent);
               OLn

            |Ast.BkConstDeclaration:
               WriteTreeImpl(C(b, Ast.ConstDeclName));
               OS("=");
               WriteTreeImpl(C(b, Ast.ConstDeclVal));
               OSemi;OLn

            |Ast.BkModule: 
               OS("MODULE "); WriteTree(C(b, Ast.ModuleName));
               OSemi;OLn;
               WriteTreeImpl(C(b, Ast.ModuleImports));
               WriteTreeImpl(C(b, Ast.ModuleDecls));
               any0 := C(b, Ast.ModuleInit);
               IF any0 # NIL THEN
                  OS("BEGIN"); 
                  INC(st.indent);
                  WriteTreeImpl(any0); 
                  DEC(st.indent); OLn
               END;
               OS("END ");WriteTreeImpl(C(b, Ast.ModuleName)); 
               OS("."); OLn

            |Ast.BkImportList:
               IF b.childLen > 0 THEN
                  OS("IMPORT");INC(st.indent);OLn;
                  WriteDelimList(b, ", ", 0, b.childLen-1);
                  DEC(st.indent);
                  OSemi;OLn
               END

            |Ast.BkPointer:
               OS("POINTER TO ");
               WriteTreeImpl(C(b, 0))

            |Ast.BkArrayDims:
               WriteDelimList(b, ", ", 0, b.childLen-1)

            |Ast.BkArrayType:
               OS("ARRAY ");
               WriteTreeImpl(C(b, Ast.ArrayTypeDims));
               OS(" OF ");
               WriteTreeImpl(C(b, Ast.ArrayTypeType))

            |Ast.BkIdentList, Ast.BkExpList:
               WriteDelimList(b, ", ", 0, b.childLen-1)

            |Ast.BkParenExpr:
               OS("(");
               WriteTreeImpl(C(b, 0));
               OS(")")

            |Ast.BkProcedureDeclaration:
               OLn;
               OS("PROCEDURE ");
               WriteTreeImpl(C(b, Ast.ProcedureDeclName));
               WriteTreeImpl(C(b, Ast.ProcedureDeclParams));
               OSemi; OLn;
               WriteTreeImpl(C(b, Ast.ProcedureDeclBody));
               OS("END ");
               WriteTerminal(C(b, Ast.ProcedureDeclName));
               OSemi;OLn


            |Ast.BkSelector:
               IF b.n = Ast.FieldAccess THEN
                  OS(".");
                  WriteTreeImpl(C(b, 0))
               ELSIF b.n = Ast.ArrayAccess THEN
                  OS("[");
                  WriteTreeImpl(C(b, 0));
                  OS("]")
               ELSIF b.n = Ast.PtrDeref THEN
                  OS("^")
               ELSIF b.n = Ast.TypeGuard THEN
                  OS("(");
                  WriteTreeImpl(C(b, 0));
                  OS(")")
               ELSE
                  Dbg.S("Unknown selector type "); Dbg.I(b.n); Dbg.Ln;
                  ASSERT(FALSE)
               END

            |Ast.BkProcedureBody:
                WriteTreeImpl(C(b, Ast.ProcBodyDecls));
                OS("BEGIN");
                INC(st.indent); 
                WriteTreeImpl(C(b, Ast.ProcBodyStmts));
                any0 := C(b, Ast.ProcBodyReturn);
                IF any0 # NIL THEN
                   OLn; OS("RETURN ");
                   WriteTreeImpl(any0)
                END;
                DEC(st.indent);OLn

            |Ast.BkImport:
               any0 := C(b, Ast.ImportAlias);
               IF any0 # NIL THEN
                  WriteTreeImpl(any0);
                  OS(":=")
               END;
               WriteTreeImpl(C(b, Ast.ImportModule))

            |Ast.BkCaseStatement:
               OS("CASE ");
               WriteTreeImpl(C(b, Ast.CaseStmtExpr));
               OS(" OF");
               INC(st.indent);OLn;
               FOR i := Ast.CaseStmtCases TO b.childLen-1 DO
                  IF i # Ast.CaseStmtCases THEN
                     OLn;OS("|")
                  END;
                  WriteTreeImpl(C(b, i))
               END;
               DEC(st.indent);OLn;
               OS("END")

            |Ast.BkCase:
               WriteTreeImpl(C(b, Ast.CaseLabelList));
               OS(":");
               INC(st.indent);
               WriteTreeImpl(C(b, Ast.CaseStmtSeq));
               DEC(st.indent)

            |Ast.BkCaseLabelList:
               WriteDelimList(b, ", ", 0, b.childLen-1)

            |Ast.BkLabelRange, Ast.BkSetElement:
               WriteTreeImpl(C(b, Ast.LabelRangeStart));
               any0 := C(b, Ast.LabelRangeEnd);
               IF any0 # NIL THEN
                  OS("..");
                  WriteTreeImpl(any0)
               END 

            |Ast.BkForStatement:
               OS("FOR ");
               WriteTreeImpl(C(b, Ast.ForStmtVarName));
               OS(" := ");
               WriteTreeImpl(C(b, Ast.ForStmtBegin));
               OS(" TO ");
               WriteTreeImpl(C(b, Ast.ForStmtEnd));
               any0 := C(b, Ast.ForStmtBy);
               IF any0 # NIL THEN
                  OS(" BY ");
                  WriteTreeImpl(any0)
               END;
               OS(" DO");
               INC(st.indent);
               WriteTreeImpl(C(b, Ast.ForStmtBody));
               DEC(st.indent);OLn;
               OS("END")

            |Ast.BkWhileStatement:
               FOR i := 0 TO b.childLen-1 BY 2 DO
                  IF i = 0 THEN
                     OS("WHILE ")
                  ELSE
                     OS("ELSIF ")
                  END;
                  WriteTreeImpl(C(b, i));
                  OS(" DO");
                  INC(st.indent);
                  WriteTreeImpl(C(b, i+1));
                  DEC(st.indent); OLn
               END;
               OS("END")

            |Ast.BkIfStmt:
               FOR i := 0 TO b.childLen-1 BY 2 DO
                  any0 := C(b, i);
                  IF any0 = NIL THEN
                     OS("ELSE")
                  ELSE
                     IF i = 0 THEN
                        OS("IF ")
                     ELSE
                        OS("ELSIF ")
                     END;
                     WriteTreeImpl(any0);
                     OS(" THEN")
                  END;
                  INC(st.indent);
                  WriteTreeImpl(C(b, i+1));
                  DEC(st.indent); OLn
               END;
               OS("END")

            |Ast.BkRepeatStatement:
               OS("REPEAT");
               INC(st.indent); 
               WriteTreeImpl(C(b, Ast.RepeatStmtBody));
               DEC(st.indent); OLn;
               OS("UNTIL ");
               WriteTreeImpl(C(b, Ast.RepeatStmtCond))

            |Ast.BkStatementSeq:
               FOR i := 0 TO b.childLen-1 DO
                  OLn;
                  WriteTreeImpl(C(b, i));
                  IF i # (b.childLen-1) THEN
                     OSemi
                  END
               END

            |Ast.BkCall:
               WriteTreeImpl(C(b, 0));
               any0 := C(b, 1);
               x := any0(Ast.Branch);
               OS("(");
               IF x.childLen > 0 THEN
                  WriteTreeImpl(any0)
               END;
               OS(")")

            |Ast.BkSet:
               OS("{");
               WriteDelimList(b, ", ", 0, b.childLen-1);
               OS("}")
         END
      END
   END
END WriteTreeImpl;

BEGIN
   WriteTree := WriteTreeImpl
END Render.

