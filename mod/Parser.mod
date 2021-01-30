(* Using the updated O7 grammar from http://oberon07.com/EBNF.txt *)
MODULE Parser;
IMPORT
   Ast, Dbg, Lex := Scanner, Strings;

CONST
   CtxRingBufSz = 16;

TYPE
   (* For debugging the parser.  A DbgCtx is added
      for every EnterCtx() call, recording 
      the current scanner position (tpos) and
      the likely Ast.Bk* Branch constant for the 
      likely production type the calling function would
      produce*)
   DbgCtx = RECORD
      col, line, prod: INTEGER
   END;

   T* = POINTER TO TDesc;
   TDesc* = RECORD
      scan*: Lex.T;
      failed: BOOLEAN;
      curFile: ARRAY 1024 OF CHAR;
      dbgCtxs: ARRAY CtxRingBufSz OF DbgCtx;
      dbci: INTEGER (* next writable position in dbgCtxs *)
   END;

VAR
   (* For breaking mutual recursion, since there aren't prototypes *)
   ParseStrucType: PROCEDURE(VAR p: T): Ast.Branch;
   ParseProcedureBody: PROCEDURE(VAR p: T): Ast.Branch;
   ParseStatementSequence: PROCEDURE(VAR p: T): Ast.Branch;
   ParseExpression: PROCEDURE(VAR p: T): Ast.T;

PROCEDURE BaseInit(p: T); 
VAR i: INTEGER;
BEGIN
   p.dbci := 0;
   FOR i := 0 TO CtxRingBufSz-1 DO
      p.dbgCtxs[i].col := -1;
      p.dbgCtxs[i].line := -1;
      p.dbgCtxs[i].prod := Ast.BkBranch
   END
END BaseInit;

PROCEDURE NewFromString*(src: ARRAY OF CHAR): T;
VAR dum: Lex.TokKind;
    p: T;
BEGIN
   NEW(p);
   BaseInit(p);
   p.scan := Lex.NewFromString(src); dum := Lex.Next(p.scan);
   p.failed := FALSE;
   RETURN p
END NewFromString;

PROCEDURE NewFromFile*(fname: ARRAY OF CHAR): T;
VAR dum: Lex.TokKind;
    p: T;
BEGIN
   NEW(p);
   BaseInit(p);
   p.scan := Lex.NewFromFile(fname); 
   dum := Lex.Next(p.scan);
   p.failed := FALSE;
   p.curFile := fname
   RETURN p
END NewFromFile;

PROCEDURE Discard(t: Lex.TokKind);
BEGIN
END Discard;

PROCEDURE PrintDbgCtx(p: T);
VAR cnt, i: INTEGER;
    finished: BOOLEAN;
BEGIN
   finished := FALSE;
   cnt := CtxRingBufSz;
   i := p.dbci - 1;
   WHILE ~finished & (cnt > 0) DO
      IF i < 0 THEN i := i + CtxRingBufSz END;
      IF p.dbgCtxs[i].line < 0 THEN
         finished := TRUE
      ELSE
         DEC(cnt);
         Dbg.S("   "); Dbg.S(Ast.BranchNames[p.dbgCtxs[i].prod]);
         Dbg.S("- "); Dbg.I(p.dbgCtxs[i].line); 
         Dbg.S(":"); Dbg.I(p.dbgCtxs[i].col); Dbg.Ln;
         DEC(i)
      END
   END
END PrintDbgCtx;

(* Call from ParseFunction with prod = the expected Ast.Bk* constant
   production type you expect to create.  Records a trail for debugging
   the parser. *)
PROCEDURE EnterCtx(VAR p: T; prod: INTEGER);
BEGIN
   IF p.failed THEN
      Dbg.S("HOLY SHIT CHIEF"); Dbg.Ln; PrintDbgCtx(p)
   END;
   p.dbgCtxs[p.dbci].col := Lex.CurCol(p.scan);
   p.dbgCtxs[p.dbci].line := p.scan.line;
   p.dbgCtxs[p.dbci].prod := prod;
   INC(p.dbci);
   IF p.dbci >= CtxRingBufSz THEN
      p.dbci := 0
   END
END EnterCtx;

PROCEDURE LeaveCtx(VAR p: T);
BEGIN
   DEC(p.dbci);
   IF p.dbci < 0 THEN p.dbci := CtxRingBufSz - 1 END;
   p.dbgCtxs[p.dbci].col := -1;
   p.dbgCtxs[p.dbci].line := -1;
   p.dbgCtxs[p.dbci].prod := Ast.BkBranch;
   IF p.dbci < 0 THEN
      p.dbci := CtxRingBufSz-1
   END
END LeaveCtx;

PROCEDURE Err(p: T; msg: ARRAY OF CHAR);
BEGIN
   Dbg.S(p.curFile);
   Dbg.S("("); Dbg.I(p.scan.line); Dbg.S(":"); Dbg.I(Lex.CurCol(p.scan));
   Dbg.S("): "); Dbg.S(msg);
   Dbg.Ln;   

   Dbg.S("DEBUG CONTEXT: "); Dbg.Ln;
   PrintDbgCtx(p);
   
   (* Our error handling is sketchy, we'd need some epic
      if then chains to check p.failed for some of the productions,
      so for now, die on the first error. *)
   ASSERT(FALSE) 
END Err;

PROCEDURE Accept(VAR p: T; t: Lex.TokKind): BOOLEAN;
VAR rv: BOOLEAN;
BEGIN
   IF p.scan.cur.kind = t THEN
      rv := TRUE;
      Discard(Lex.Next(p.scan))
   ELSE
      rv := FALSE
   END
   RETURN rv
END Accept;

PROCEDURE AcceptOrFail(VAR p: T; t: Lex.TokKind): BOOLEAN;
VAR buf: ARRAY 128 OF CHAR;
    rv: BOOLEAN;
BEGIN
   rv := Accept(p, t);
   IF ~rv THEN
      p.failed := TRUE;
      buf := "Was expecting ";
      Strings.Append(Lex.TokenNames[t], buf);
      Strings.Append(" but got instead: ", buf);
      Strings.Append(Lex.TokenNames[p.scan.cur.kind], buf);
      Err(p, buf)
   END
   RETURN rv
END AcceptOrFail;

(* AcceptOrFail without the return *)
PROCEDURE MustAccept(VAR p: T; t: Lex.TokKind);
BEGIN
   IF ~AcceptOrFail(p, t) THEN END
END MustAccept;

(* Adds a NIL child to a branch, for optional productions *)
PROCEDURE NilSlot(b: Ast.Branch);
BEGIN
   Ast.AddChild(b, NIL)
END NilSlot;

PROCEDURE ParseIdent(VAR p: T): Ast.Terminal;
VAR rv: Ast.Terminal;
BEGIN
   IF AcceptOrFail(p, Lex.Id) THEN
      rv := Ast.MkTerminal(p.scan.prev)
   END
   RETURN rv
END ParseIdent;

(* [ident "."] ident. *)
PROCEDURE ParseQualIdent*(VAR p: T): Ast.Branch;
VAR rv: Ast.Branch;
    i0, i1: Ast.Terminal;
BEGIN
   EnterCtx(p, Ast.BkQualIdent);
   rv := NIL;
   IF ~p.failed THEN
      i0 := ParseIdent(p);
      IF i0 # NIL THEN
         rv := Ast.MkQualIdent();
         IF Accept(p, Lex.DOT) THEN
            i1 := ParseIdent(p);
            IF i1 # NIL THEN
               (* Qualified *)
               Ast.AddChild(rv, i0);
               Ast.AddChild(rv, i1)
            ELSE
               rv := NIL
            END
         ELSE
            (* Unqualified identifier, NIL module *)
            NilSlot(rv);
            Ast.AddChild(rv, i0)
         END
      END
   END;
   LeaveCtx(p)
   RETURN rv
END ParseQualIdent;


PROCEDURE CurTok(p: T): Lex.TokKind;
BEGIN
   RETURN p.scan.cur.kind
END CurTok;

(* ident ["*"] *)
PROCEDURE ParseIdentDef(VAR p: T): Ast.Terminal;
VAR rv: Ast.Terminal;
BEGIN
   rv := ParseIdent(p);
   IF Accept(p, Lex.ASTERISK) THEN
      rv.export := TRUE
   END;
   RETURN rv
END ParseIdentDef;

PROCEDURE ParseConstExpression(VAR p: T): Ast.T;
BEGIN
   RETURN ParseExpression(p)
END ParseConstExpression;

(* identdef "=" ConstExpression. *)
PROCEDURE ParseConstDeclaration(VAR p: T): Ast.Branch;
VAR rv: Ast.Branch;
BEGIN
   EnterCtx(p, Ast.BkConstDeclaration);
   rv := Ast.MkConstDeclaration();
   Ast.AddChild(rv, ParseIdentDef(p));
   MustAccept(p, Lex.EQ);
   Ast.AddChild(rv, ParseConstExpression(p));
   IF p.failed THEN rv := NIL END;
   LeaveCtx(p);
   RETURN rv
END ParseConstDeclaration;

(* qualident | StrucType *)
PROCEDURE ParseType(VAR p: T): Ast.Branch;
VAR rv: Ast.Branch;
BEGIN
   IF CurTok(p) = Lex.Id THEN
      rv := ParseQualIdent(p)
   ELSE
      rv := ParseStrucType(p)
   END;
   RETURN rv
END ParseType;

(* ARRAY length {"," length} OF type *)
PROCEDURE ParseArrayType(VAR p: T): Ast.Branch;
VAR rv, dims: Ast.Branch;
BEGIN
   EnterCtx(p, Ast.BkBranch);
   IF AcceptOrFail(p, Lex.KARRAY) THEN
      dims := Ast.MkArrayDims();
      REPEAT
         Ast.AddChild(dims, ParseExpression(p))
      UNTIL p.failed OR ~Accept(p, Lex.COMMA);
      IF AcceptOrFail(p, Lex.KOF) THEN
         rv := Ast.MkArrayType();
         Ast.AddChild(rv, dims);
         Ast.AddChild(rv, ParseType(p))
      END
   END;
   IF p.failed THEN rv := NIL END;
   LeaveCtx(p);
   RETURN rv
END ParseArrayType;

(* identdef {"," identdef} *)
PROCEDURE ParseIdentList(VAR p: T): Ast.Branch;
VAR rv: Ast.Branch;
BEGIN
   EnterCtx(p, Ast.BkBranch);
   rv := Ast.MkIdentList();
   REPEAT
      Ast.AddChild(rv, ParseIdentDef(p))
   UNTIL p.failed OR ~Accept(p, Lex.COMMA);
   IF p.failed THEN rv := NIL END;
   LeaveCtx(p);
   RETURN rv
END ParseIdentList;

(* IdentList ":" type *)
PROCEDURE ParseFieldList(VAR p: T): Ast.Branch;
VAR rv: Ast.Branch;
BEGIN
   EnterCtx(p, Ast.BkFieldList);
   rv := Ast.MkFieldList();
   Ast.AddChild(rv, ParseIdentList(p));
   IF AcceptOrFail(p, Lex.COLON) THEN
      Ast.AddChild(rv, ParseType(p))
   END;
   IF p.failed THEN rv := NIL END;
   LeaveCtx(p);
   RETURN rv
END ParseFieldList;

(* FieldList {";" FieldList}. *)
PROCEDURE ParseFieldListSequence(VAR p: T): Ast.Branch;
VAR rv: Ast.Branch;
BEGIN
   rv := Ast.MkFieldListSequence();
   IF CurTok(p) = Lex.Id THEN
      REPEAT
         Ast.AddChild(rv, ParseFieldList(p))
      UNTIL p.failed OR ~Accept(p, Lex.SEMI)
   END
   RETURN rv
END ParseFieldListSequence;

(* RECORD ["(" BaseType ")"] [FieldListSequence] END. *)
PROCEDURE ParseRecordType(VAR p: T): Ast.Branch;
VAR rv: Ast.Branch;
BEGIN
   EnterCtx(p, Ast.BkRecordType);
   rv := NIL;
   IF AcceptOrFail(p, Lex.KRECORD) THEN
      rv := Ast.MkRecordType();
      IF Accept(p, Lex.LPAREN) THEN
         Ast.AddChild(rv, ParseQualIdent(p));
         MustAccept(p, Lex.RPAREN)
      ELSE
         NilSlot(rv)
      END;
      Ast.AddChild(rv, ParseFieldListSequence(p));
      MustAccept(p, Lex.KEND)
   END;
   IF p.failed THEN rv := NIL END;
   LeaveCtx(p);

   RETURN rv
END ParseRecordType;

(* POINTER TO type *)
PROCEDURE ParsePointerType(VAR p: T): Ast.Branch;
VAR rv: Ast.Branch;
BEGIN
   EnterCtx(p, Ast.BkPointer);
   IF AcceptOrFail(p, Lex.KPOINTER) & AcceptOrFail(p, Lex.KTO) THEN
      rv := Ast.MkPointer();
      Ast.AddChild(rv, ParseType(p))
   END;
   IF p.failed THEN rv := NIL END;
   LeaveCtx(p);
   RETURN rv
END ParsePointerType;

(* {ARRAY OF} qualident. *)
PROCEDURE ParseFormalType(VAR p: T): Ast.Branch;
VAR rv: Ast.Branch;
BEGIN
   EnterCtx(p, Ast.BkFormalType);
   rv := Ast.MkFormalType();
   WHILE Accept(p, Lex.KARRAY) & AcceptOrFail(p, Lex.KOF) DO
      INC(rv.n) 
   END;
   Ast.AddChild(rv, ParseQualIdent(p));
   IF p.failed THEN rv := NIL END;
   LeaveCtx(p);
   RETURN rv
END ParseFormalType;

(* [VAR] ident {"," ident} ":" FormalType. *)
PROCEDURE ParseFPSection(VAR p: T): Ast.Branch;
VAR rv: Ast.Branch;
BEGIN
   EnterCtx(p, Ast.BkFPSection);
   rv := Ast.MkFPSection();
   IF Accept(p, Lex.KVAR) THEN INCL(rv.flags, Ast.NfVar) END;
   REPEAT
      Ast.AddChild(rv, ParseIdent(p))
   UNTIL p.failed OR ~Accept(p, Lex.COMMA);
   MustAccept(p, Lex.COLON);
   Ast.AddChild(rv, ParseFormalType(p));
   IF p.failed THEN rv := NIL END;
   LeaveCtx(p);
   RETURN rv
END ParseFPSection;
   

(* "(" [FPSection {";" FPSection}] ")" [":" qualident] *)
PROCEDURE ParseFormalParameters(VAR p: T): Ast.Branch;
VAR rv: Ast.Branch;
    tk: Lex.TokKind;
BEGIN
   EnterCtx(p, Ast.BkFormalParameters);
   IF AcceptOrFail(p, Lex.LPAREN) THEN
      rv := Ast.MkFormalParameters();
      NilSlot(rv); (* placeholder for return type *)
      tk := CurTok(p);
      IF (tk = Lex.KVAR) OR (tk = Lex.Id) THEN
         REPEAT
            Ast.AddChild(rv, ParseFPSection(p))
         UNTIL p.failed OR ~Accept(p, Lex.SEMI)
      END;
      MustAccept(p, Lex.RPAREN);
      IF Accept(p, Lex.COLON) THEN
         Ast.SetChild(rv, 0, ParseQualIdent(p))
      END
   END;
   IF p.failed THEN rv := NIL END;
   LeaveCtx(p);

   RETURN rv
END ParseFormalParameters;

(* PROCEDURE [FormalParameters] *)
PROCEDURE ParseProcedureType*(VAR p: T): Ast.Branch;
VAR rv: Ast.Branch;
BEGIN
   EnterCtx(p, Ast.BkProcedureType);
   IF AcceptOrFail(p, Lex.KPROCEDURE) THEN
      rv := Ast.MkProcedureType();
      Ast.AddChild(rv, ParseFormalParameters(p))
   END;
   IF p.failed THEN rv := NIL END;
   LeaveCtx(p);
   RETURN rv
END ParseProcedureType;

(* StrucType = ArrayType | RecordType | PointerType | ProcedureType *)
(* NB - so it looks like in the updated spec, you can't do a type
        alias, like Bork = Ast.T.  I need to look at more code and see
        how common this is, but for now I will accept it because I use
        an alias like this for Scanner.TokKind, which obnc obviously 
        accepts *)
PROCEDURE ParseStrucTypeImpl(VAR p: T): Ast.Branch;
VAR rv: Ast.Branch;
    t: Lex.TokKind;
BEGIN
   rv := NIL;
   t := CurTok(p);
   IF t = Lex.KARRAY THEN
      rv := ParseArrayType(p)
   ELSIF t = Lex.KRECORD THEN
      rv := ParseRecordType(p)
   ELSIF t = Lex.KPOINTER THEN
      rv := ParsePointerType(p)
   ELSIF t = Lex.KPROCEDURE THEN
      rv := ParseProcedureType(p)
   ELSIF t = Lex.Id THEN
      (* See NB above function decl *)
      rv := ParseQualIdent(p)
   ELSE
      Err(p, "Was expecting ARRAY, RECORD, POINTER or PROCEDURE.")
   END;

   RETURN rv
END ParseStrucTypeImpl;

(* identdef "=" StrucType *)
PROCEDURE ParseTypeDeclaration(VAR p: T): Ast.Branch;
VAR rv: Ast.Branch;
BEGIN
   EnterCtx(p, Ast.BkTypeDeclaration);
   rv := Ast.MkTypeDeclaration();
   Ast.AddChild(rv, ParseIdentDef(p));
   MustAccept(p, Lex.EQ);
   Ast.AddChild(rv, ParseStrucType(p));
   IF p.failed THEN rv := NIL END;
   LeaveCtx(p);
   RETURN rv
END ParseTypeDeclaration;

(* IdentList ":" type. *)
PROCEDURE ParseVarDeclaration(VAR p: T): Ast.Branch;
VAR rv: Ast.Branch;
BEGIN
   EnterCtx(p, Ast.BkVarDeclaration);
   rv := Ast.MkVarDeclaration();
   Ast.AddChild(rv, ParseIdentList(p));
   MustAccept(p, Lex.COLON);
   Ast.AddChild(rv, ParseType(p));
   IF p.failed THEN rv := NIL END;
   LeaveCtx(p);
   RETURN rv
END ParseVarDeclaration;

(* ProcdedureHeading := PROCEDURE identdef [FormalParameters] *)
(* ProcedureHeading ";" ProcedureBody ident *)
PROCEDURE ParseProcedureDeclaration(VAR p: T): Ast.Branch;
VAR rv: Ast.Branch;
    name0, name1: Ast.Terminal;
BEGIN
   EnterCtx(p, Ast.BkProcedureDeclaration);
   IF AcceptOrFail(p, Lex.KPROCEDURE) THEN
      rv := Ast.MkProcedureDeclaration();
      name0 := ParseIdentDef(p);
      Ast.AddChild(rv, name0);
      IF CurTok(p) = Lex.LPAREN THEN
         Ast.AddChild(rv, ParseFormalParameters(p))
      ELSE
         NilSlot(rv)
      END;
      MustAccept(p, Lex.SEMI);
      Ast.AddChild(rv, ParseProcedureBody(p));
      name1 := ParseIdentDef(p);
      IF ~Lex.Eql(p.scan, name0.tok, name1.tok) THEN
         Err(p, "END ident does not match procedure name.")
      END
   END;
   IF p.failed THEN rv := NIL END;
   LeaveCtx(p);
   RETURN rv
END ParseProcedureDeclaration;


(* DeclarationSequence = [CONST {ConstDeclaration ";"}]
                         [TYPE {TypeDeclaration ";"}] [VAR {VariableDeclaration ";"}]
                         {ProcedureDeclaration ";"} *)
PROCEDURE ParseDeclarationSequence(VAR p: T): Ast.Branch;
VAR rv, sc: Ast.Branch;
BEGIN
   EnterCtx(p, Ast.BkDeclarationSequence);
   rv := Ast.MkDeclarationSequence();
   IF Accept(p, Lex.KCONST) THEN
      sc := Ast.MkConstDeclSeq();
      Ast.AddChild(rv, sc);
      EnterCtx(p, Ast.BkConstDeclSeq);
      WHILE ~p.failed & (CurTok(p) = Lex.Id) DO 
         Ast.AddChild(sc, ParseConstDeclaration(p));
         MustAccept(p, Lex.SEMI)
      END;
      LeaveCtx(p)
   ELSE
      NilSlot(rv)
   END;

   IF ~p.failed & Accept(p, Lex.KTYPE) THEN
      sc := Ast.MkTypeDeclSeq();
      Ast.AddChild(rv, sc);
      EnterCtx(p, Ast.BkTypeDeclSeq);
      WHILE ~p.failed & (CurTok(p) = Lex.Id) DO
         Ast.AddChild(sc, ParseTypeDeclaration(p));
         MustAccept(p, Lex.SEMI)
      END;
      LeaveCtx(p)
   ELSE
      NilSlot(rv)
   END;

   IF ~p.failed & Accept(p, Lex.KVAR) THEN
      sc := Ast.MkVarDeclSeq();
      Ast.AddChild(rv, sc);
      EnterCtx(p, Ast.BkVarDeclSeq);
      WHILE ~p.failed & (CurTok(p) = Lex.Id) DO
         Ast.AddChild(sc, ParseVarDeclaration(p));
         MustAccept(p, Lex.SEMI)
      END;
      LeaveCtx(p)
   ELSE
      NilSlot(rv)
   END;

   sc := Ast.MkProcDeclSeq();
   Ast.AddChild(rv, sc);
   EnterCtx(p, Ast.BkProcDeclSeq);
   WHILE ~p.failed & (CurTok(p) = Lex.KPROCEDURE) DO
      Ast.AddChild(sc, ParseProcedureDeclaration(p));
      MustAccept(p, Lex.SEMI)
   END;
   LeaveCtx(p);
   LeaveCtx(p);
   RETURN rv
END ParseDeclarationSequence;

(* expression {"," expression}. *)
PROCEDURE ParseExpList(VAR p:T): Ast.Branch;
VAR rv: Ast.Branch;
BEGIN
   EnterCtx(p, Ast.BkExpList);
   rv := Ast.MkExpList();
   REPEAT
      Ast.AddChild(rv, ParseExpression(p))
   UNTIL p.failed OR ~Accept(p, Lex.COMMA);
   IF p.failed THEN rv := NIL END;
   LeaveCtx(p);
   RETURN rv
END ParseExpList; 

(* "(" [ExpList] ")". *)
PROCEDURE ParseActualParameters(VAR p:T): Ast.Branch;
VAR rv: Ast.Branch;
BEGIN
   rv := NIL;
   IF Accept(p, Lex.LPAREN) THEN
      IF CurTok(p) # Lex.RPAREN THEN
         rv := ParseExpList(p)
      ELSE 
         rv := Ast.MkExpList()
      END;
      MustAccept(p, Lex.RPAREN)
   END;
   RETURN rv
END ParseActualParameters; 

(* element = expression [".." expression]. *)
(*  "{" [element {"," element}] "} *)
PROCEDURE ParseSet(VAR p:T): Ast.Branch;
VAR rv, el: Ast.Branch;
BEGIN
   EnterCtx(p, Ast.BkSet);
   IF AcceptOrFail(p, Lex.LBRACE) THEN
      rv := Ast.MkSet();
      IF CurTok(p) # Lex.RBRACE THEN
         REPEAT
            el := Ast.MkSetElement();
            Ast.AddChild(el, ParseExpression(p));
            IF Accept(p, Lex.DOTDOT) THEN
               Ast.AddChild(el, ParseExpression(p))
            ELSE
               NilSlot(el)
            END;
            Ast.AddChild(rv, el)
         UNTIL p.failed OR ~Accept(p, Lex.COMMA)
      END;
      MustAccept(p, Lex.RBRACE)
   END;
   IF p.failed THEN rv := NIL END;
   LeaveCtx(p);
   RETURN rv
END ParseSet; 

(* qualident {selector}. *)
(* selector = "." ident | "[" ExpList "]" | "^" | "(" qualident ")" *)
PROCEDURE ParseDesignator(VAR p:T): Ast.Branch;
VAR rv, s: Ast.Branch;
    pos: Lex.LexState;
    ignoreLParen: BOOLEAN;
BEGIN
   EnterCtx(p, Ast.BkDesignator);
   rv := Ast.MkDesignator();
   Ast.AddChild(rv, ParseQualIdent(p));
   ignoreLParen := FALSE;
   WHILE Accept(p, Lex.DOT) DO
      s := Ast.MkSelector(Ast.FieldAccess);
      Ast.AddChild(s, ParseIdent(p));
      Ast.AddChild(rv, s)
   ELSIF Accept(p, Lex.LBRACKET) DO
      s := Ast.MkSelector(Ast.ArrayAccess);
      Ast.AddChild(s, ParseExpList(p));
      MustAccept(p, Lex.RBRACKET);
      Ast.AddChild(rv, s)
   ELSIF Accept(p, Lex.CARET) DO
      Ast.AddChild(rv, Ast.MkSelector(Ast.PtrDeref))
   ELSIF ~ignoreLParen & (CurTok(p) = Lex.LPAREN) DO
      (* Note, there's a ambiguity in the grammar where
         a function call with a single parameter can get
         parsed as a TypeGuard selector.  For the cases
         that match, this can be fixed up in semcheck once
         we're building symbol tables, but we need to make
         sure we're not too eager here and barf on a real
         function call that has more parameters *)
      Lex.Mark(p.scan, pos);
      MustAccept(p, Lex.LPAREN);
      IF CurTok(p) = Lex.Id THEN
         s := Ast.MkSelector(Ast.TypeGuard);
         Ast.AddChild(s, ParseQualIdent(p));
         IF Accept(p, Lex.RPAREN) THEN
            Ast.AddChild(rv, s)
         ELSE
            ignoreLParen := TRUE;
            Lex.Rewind(p.scan, pos);
            p.failed := FALSE
         END
      ELSE
         ignoreLParen := TRUE;
         Lex.Rewind(p.scan, pos);
         p.failed := FALSE
      END
   END;
   IF p.failed THEN rv := NIL END;
   LeaveCtx(p);
   RETURN rv
END ParseDesignator; 

(* ForStatement = FOR ident ":=" expression TO expression [BY ConstExpression]
    DO StatementSequence END *)
PROCEDURE ParseForStatement(VAR p:T): Ast.Branch;
VAR rv: Ast.Branch;
BEGIN
   EnterCtx(p, Ast.BkForStatement);
   IF AcceptOrFail(p, Lex.KFOR) THEN
      rv := Ast.MkForStatement();
      Ast.AddChild(rv, ParseIdent(p));
      MustAccept(p, Lex.COLEQ);
      Ast.AddChild(rv, ParseExpression(p));
      MustAccept(p, Lex.KTO);
      Ast.AddChild(rv, ParseExpression(p));
      IF Accept(p, Lex.KBY) THEN
         Ast.AddChild(rv, ParseConstExpression(p))
      ELSE
         NilSlot(rv)
      END;
      MustAccept(p, Lex.KDO);
      Ast.AddChild(rv, ParseStatementSequence(p));
      MustAccept(p, Lex.KEND)
   END;
   IF p.failed THEN rv := NIL END;
   LeaveCtx(p);
   RETURN rv

END ParseForStatement; 

(* RepeatStatement = REPEAT StatementSequence UNTIL expression. *)
PROCEDURE ParseRepeatStatement(VAR p:T): Ast.Branch;
VAR rv: Ast.Branch;
BEGIN
   EnterCtx(p, Ast.BkRepeatStatement);
   IF AcceptOrFail(p, Lex.KREPEAT) THEN
      rv := Ast.MkRepeatStatement();
      Ast.AddChild(rv, ParseStatementSequence(p));
      MustAccept(p, Lex.KUNTIL);
      Ast.AddChild(rv, ParseExpression(p))
   END;
   IF p.failed THEN rv := NIL END;
   LeaveCtx(p);
   RETURN rv
END ParseRepeatStatement; 

(* WhileStatement = WHILE expression DO StatementSequence
    {ELSIF expression DO StatementSequence} END *)
PROCEDURE ParseWhileStatement(VAR p:T): Ast.Branch;
VAR rv: Ast.Branch;
BEGIN
   EnterCtx(p, Ast.BkWhileStatement);
   IF AcceptOrFail(p, Lex.KWHILE) THEN
      rv := Ast.MkWhileStatement();
      REPEAT
         Ast.AddChild(rv, ParseExpression(p));
         MustAccept(p, Lex.KDO);
         Ast.AddChild(rv, ParseStatementSequence(p))
      UNTIL p.failed OR ~Accept(p, Lex.KELSIF);
      MustAccept(p, Lex.KEND)
   END;
   IF p.failed THEN rv := NIL END;
   LeaveCtx(p);
   RETURN rv
END ParseWhileStatement; 

(* CaseStatement = CASE expression OF case {"|" case} END.
   case          = [CaseLabelList ":" StatementSequence].
   CaseLabelList = LabelRange {"," LabelRange}.
   LabelRange    = label [".." label].
   label = integer | string | qualident. *)
PROCEDURE ParseLabel(VAR p:T): Ast.T;
VAR rv: Ast.T;
    t: Lex.TokKind;
BEGIN
   t := CurTok(p);
   IF (t = Lex.ConstInt) OR (t = Lex.ConstHex) 
         OR (t = Lex.ConstString) OR (t = Lex.ConstHexString) THEN
      Discard(Lex.Next(p.scan));
      rv := Ast.MkTerminal(p.scan.prev)
   ELSIF t = Lex.Id THEN  
      rv := ParseQualIdent(p)
   ELSE
      Err(p, "Expecting literal int, string or qualified type")
   END;
   IF p.failed THEN rv := NIL END;
   RETURN rv 
END ParseLabel; 

PROCEDURE ParseLabelRange(VAR p:T): Ast.Branch;
VAR rv: Ast.Branch;
BEGIN
   EnterCtx(p, Ast.BkLabelRange);
   rv := Ast.MkLabelRange();
   Ast.AddChild(rv, ParseLabel(p));
   IF Accept(p, Lex.DOTDOT) THEN
      Ast.AddChild(rv, ParseLabel(p))
   END;
   IF p.failed THEN rv := NIL END;
   LeaveCtx(p);
   RETURN rv
END ParseLabelRange; 

PROCEDURE ParseCaseLabelList(VAR p:T): Ast.Branch;
VAR rv: Ast.Branch;
BEGIN
   EnterCtx(p, Ast.BkCaseLabelList);
   rv := Ast.MkCaseLabelList();
   REPEAT
      Ast.AddChild(rv, ParseLabelRange(p))
   UNTIL p.failed OR ~Accept(p, Lex.COMMA);
   IF p.failed THEN rv := NIL END;
   LeaveCtx(p);
   RETURN rv
END ParseCaseLabelList; 
   
PROCEDURE ParseCase(VAR p:T): Ast.Branch;
VAR rv: Ast.Branch;
BEGIN
   EnterCtx(p, Ast.BkCase);
   rv := NIL;
   IF CurTok(p) # Lex.KEND THEN
      rv := Ast.MkCase();
      Ast.AddChild(rv, ParseCaseLabelList(p));
      MustAccept(p, Lex.COLON);
      Ast.AddChild(rv, ParseStatementSequence(p))
   END;
   IF p.failed THEN rv := NIL END;
   LeaveCtx(p);
   RETURN rv
END ParseCase; 

PROCEDURE ParseCaseStatement(VAR p:T): Ast.Branch;
VAR rv: Ast.Branch;
BEGIN
   EnterCtx(p, Ast.BkCaseStatement);
   IF Accept(p, Lex.KCASE) THEN
      rv := Ast.MkCaseStatement();
      Ast.AddChild(rv, ParseExpression(p));
      MustAccept(p, Lex.KOF);
      IF CurTok(p) # Lex.KEND THEN
         REPEAT
            Ast.AddChild(rv, ParseCase(p))
         UNTIL p.failed OR ~Accept(p, Lex.BAR)
      END;
      MustAccept(p, Lex.KEND)
   END;
   IF p.failed THEN rv := NIL END;
   LeaveCtx(p);
   RETURN rv
END ParseCaseStatement; 

(* IfStatement = IF expression THEN StatementSequence
    {ELSIF expression THEN StatementSequence}
        [ELSE StatementSequence] END *)
PROCEDURE ParseIfStatement(VAR p:T): Ast.Branch;
VAR rv: Ast.Branch;
BEGIN
   EnterCtx(p, Ast.BkIfStmt);
   IF AcceptOrFail(p, Lex.KIF) THEN
      rv := Ast.MkIfStmt();
      REPEAT
         Ast.AddChild(rv, ParseExpression(p));
         MustAccept(p, Lex.KTHEN);
         Ast.AddChild(rv, ParseStatementSequence(p))
      UNTIL p.failed OR ~Accept(p, Lex.KELSIF);

      IF Accept(p, Lex.KELSE) THEN
         NilSlot(rv);
         Ast.AddChild(rv, ParseStatementSequence(p))
      END;
      MustAccept(p, Lex.KEND)
   END;
   IF p.failed THEN rv := NIL END;
   LeaveCtx(p);
   RETURN rv
END ParseIfStatement; 

(* assignment = designator ":=" expression. *)
(* ProcedureCall = designator [ActualParameters] *)
(* [assignment | ProcedureCall | IfStatement | CaseStatement |
    WhileStatement | RepeatStatement | ForStatement] *)
PROCEDURE ParseStatement*(VAR p:T): Ast.Branch;
VAR rv, desig: Ast.Branch;
    t: Lex.TokKind;
BEGIN
   t := CurTok(p);
   IF t = Lex.Id THEN
      (* assignments and procedure calls both start with designators, so
         deal with those in pieces *)
      desig := ParseDesignator(p);
      IF Accept(p, Lex.COLEQ) THEN
         rv := Ast.MkBinOp();
         Ast.AddChild(rv, desig);
         Ast.AddChild(rv, Ast.MkTerminal(p.scan.prev));
         Ast.AddChild(rv, ParseExpression(p))
      ELSIF CurTok(p) = Lex.LPAREN THEN
         rv := Ast.MkCall();
         Ast.AddChild(rv, desig);
         Ast.AddChild(rv, ParseActualParameters(p))
      ELSE
         (* Probably a function call without params, or the designator
            greedily ate the function call params as a TypeGuard.  
            Both will be resoved for SymCheck *)
         rv := desig
      END
   ELSIF t = Lex.KIF THEN
      rv := ParseIfStatement(p)
   ELSIF t = Lex.KCASE THEN
      rv := ParseCaseStatement(p)
   ELSIF t = Lex.KWHILE THEN
      rv := ParseWhileStatement(p)
   ELSIF t = Lex.KREPEAT THEN
      rv := ParseRepeatStatement(p)
   ELSIF t = Lex.KFOR THEN
      rv := ParseForStatement(p)
   ELSE
      Err(p, "Was expecting a statement.")
   END;
   IF p.failed THEN rv := NIL END;
   RETURN rv
END ParseStatement; 

(* StatementSequence = statement {";" statement} *)
(* NB - not in the grammar, we do extra checks to allow 
   StatementSequence to terminate after the semicolon that
   divides the end of the sequence from the closing RETURN
   statement for the procedure body. We were already looking
   for RETURN anyway to detect empty statement sequences. *)
PROCEDURE ParseStatementSequenceImpl(VAR p: T): Ast.Branch;
VAR rv: Ast.Branch;
    hitReturn: BOOLEAN;
BEGIN
   EnterCtx(p, Ast.BkStatementSeq);
   rv := Ast.MkStatementSeq();
   IF (CurTok(p) # Lex.KEND) & (CurTok(p) # Lex.KRETURN) THEN
      hitReturn := FALSE;
      REPEAT
         IF CurTok(p) = Lex.KRETURN THEN
            hitReturn := TRUE
         ELSE
            Ast.AddChild(rv, ParseStatement(p))
         END
      UNTIL hitReturn OR p.failed OR ~Accept(p, Lex.SEMI)
   END;
   IF p.failed THEN rv := NIL END;
   LeaveCtx(p);
   RETURN rv
END ParseStatementSequenceImpl;

(* number | string | NIL | TRUE | FALSE |
    set | designator [ActualParameters] | "(" expression ")" | "~" factor. *)
PROCEDURE ParseFactor(VAR p:T): Ast.T;
VAR rv: Ast.T;
    b: Ast.Branch;
    t: Lex.TokKind;
BEGIN
   t := CurTok(p);
   IF (t = Lex.ConstInt) OR (t = Lex.ConstReal) OR
         (t = Lex.ConstHex) OR (t = Lex.ConstString) OR
         (t = Lex.ConstHexString) OR (t = Lex.KNIL) OR
         (t = Lex.KTRUE) OR (t = Lex.KFALSE) THEN
      Discard(Lex.Next(p.scan));
      rv := Ast.MkTerminal(p.scan.prev)
   ELSIF t = Lex.LBRACE THEN
      rv := ParseSet(p)
   ELSIF t = Lex.Id THEN
      rv := ParseDesignator(p);
      IF CurTok(p) = Lex.LPAREN THEN
         b := Ast.MkCall();
         Ast.AddChild(b, rv);
         Ast.AddChild(b, ParseActualParameters(p));
         rv := b
      END
   ELSIF Accept(p, Lex.LPAREN) THEN
      b := Ast.MkParenExpr();
      Ast.AddChild(b, ParseExpression(p));
      rv := b;
      MustAccept(p, Lex.RPAREN)
   ELSIF Accept(p, Lex.TILDE) THEN
      b := Ast.MkUnOp();
      Ast.AddChild(b, Ast.MkTerminal(p.scan.prev));
      Ast.AddChild(b, ParseFactor(p));
      rv := b
   ELSE
      Err(p, "Was expecting a literal string/number/set, '(' or '~'")
   END;
   IF p.failed THEN rv := NIL END;
   RETURN rv
END ParseFactor; 

(* "*" | "/" | DIV | MOD | "&" *)
PROCEDURE AcceptMulOperator(VAR p:T): BOOLEAN;
BEGIN
   RETURN Accept(p, Lex.ASTERISK) OR Accept(p, Lex.FSLASH) OR
          Accept(p, Lex.KDIV) OR Accept(p, Lex.KMOD) OR
          Accept(p, Lex.AMPERSAND)
END AcceptMulOperator; 

(* factor {MulOperator factor} *)
PROCEDURE ParseTerm(VAR p: T): Ast.T;
VAR rv: Ast.T;
    b: Ast.Branch;
BEGIN
   rv := ParseFactor(p);
   WHILE ~p.failed & AcceptMulOperator(p) DO
      b := Ast.MkBinOp();
      Ast.AddChild(b, rv);
      Ast.AddChild(b, Ast.MkTerminal(p.scan.prev));
      Ast.AddChild(b, ParseFactor(p));
      rv := b
   END;
   IF p.failed THEN rv := NIL END;
   RETURN rv
END ParseTerm;

(* "+" | "-" | OR *)
PROCEDURE AcceptAddOperator(VAR p: T): BOOLEAN;
BEGIN
   RETURN Accept(p, Lex.PLUS) OR Accept(p, Lex.MINUS) OR Accept(p, Lex.KOR)
END AcceptAddOperator;

(* ["+" | "-"] term {AddOperator term}. *)
PROCEDURE ParseSimpleExpression(VAR p: T): Ast.T;
VAR rv: Ast.T;
    b: Ast.Branch;
BEGIN
   IF Accept(p, Lex.PLUS) OR Accept(p, Lex.MINUS) THEN
      b := Ast.MkUnOp();
      Ast.AddChild(b, Ast.MkTerminal(p.scan.prev));
      Ast.AddChild(b, ParseTerm(p));
      rv := b
   ELSE
      rv := ParseTerm(p)
   END;
   WHILE ~p.failed & AcceptAddOperator(p) DO
      b := Ast.MkBinOp();
      Ast.AddChild(b, rv);
      Ast.AddChild(b, Ast.MkTerminal(p.scan.prev));
      Ast.AddChild(b, ParseTerm(p));
      rv := b
   END;
   IF p.failed THEN rv := NIL END;
   RETURN rv
END ParseSimpleExpression;

(* "=" | "#" | "<" | "<=" | ">" | ">=" | IN | IS *)
PROCEDURE AcceptRelation(VAR p: T): BOOLEAN;
BEGIN
   RETURN Accept(p, Lex.EQ) OR Accept(p, Lex.OCTOTHORPE) OR Accept(p, Lex.LT)
          OR Accept(p, Lex.LTE) OR Accept(p, Lex.GT) OR Accept(p, Lex.GTE) 
          OR Accept(p, Lex.KIN) OR Accept(p, Lex.KIS)
END AcceptRelation;

(* SimpleExpression [relation SimpleExpression] *)
PROCEDURE ParseExpressionImpl(VAR p: T): Ast.T;
VAR rv: Ast.T;
    b: Ast.Branch;
BEGIN
   rv := ParseSimpleExpression(p);
   IF AcceptRelation(p) THEN
      b := Ast.MkBinOp();
      Ast.AddChild(b, rv); 
      Ast.AddChild(b, Ast.MkTerminal(p.scan.prev));
      Ast.AddChild(b, ParseSimpleExpression(p));
      rv := b
   END;
   IF p.failed THEN rv := NIL END;
   RETURN rv
END ParseExpressionImpl;

   
(* DeclarationSequence [BEGIN StatementSequence]
    [RETURN expression] END *)
PROCEDURE ParseProcedureBodyImpl(VAR p: T): Ast.Branch;
VAR rv: Ast.Branch;
BEGIN
   EnterCtx(p, Ast.BkProcedureBody);
   rv := Ast.MkProcedureBody();
   Ast.AddChild(rv, ParseDeclarationSequence(p));
   IF Accept(p, Lex.KBEGIN) THEN
      Ast.AddChild(rv, ParseStatementSequence(p))
   END;
   IF Accept(p, Lex.KRETURN) THEN
      Ast.AddChild(rv, ParseExpression(p))
   END;
   MustAccept(p, Lex.KEND);
   IF p.failed THEN rv := NIL END;
   LeaveCtx(p);
   RETURN rv
END ParseProcedureBodyImpl;


(* import = ident [":=" ident] *)
PROCEDURE ParseImport(VAR p: T): Ast.Branch;
VAR rv: Ast.Branch;
    t0: Ast.Terminal;
BEGIN
   EnterCtx(p, Ast.BkImport);
   rv := NIL;
   t0 := ParseIdent(p);
   IF t0 # NIL THEN
      rv := Ast.MkImport();
      IF Accept(p, Lex.COLEQ) THEN
         Ast.AddChild(rv, t0);
         Ast.AddChild(rv, ParseIdent(p))
      ELSE
         NilSlot(rv);
         Ast.AddChild(rv, t0)
      END
   END;
   LeaveCtx(p);
   RETURN rv
END ParseImport;

(* ImportList = IMPORT import {"," import} ";" *)
PROCEDURE ParseImportList(VAR p: T): Ast.Branch;
VAR rv: Ast.Branch;
BEGIN
   EnterCtx(p, Ast.BkImportList);
   IF AcceptOrFail(p, Lex.KIMPORT) THEN
      rv := Ast.MkImportList();
      REPEAT
         Ast.AddChild(rv, ParseImport(p))
      UNTIL p.failed OR ~Accept(p, Lex.COMMA);
      MustAccept(p, Lex.SEMI)
   END;
   LeaveCtx(p);
   RETURN rv
END ParseImportList;


(* module = MODULE ident ";" [ImportList] DeclarationSequence
            [BEGIN StatementSequence] END ident "." *)
PROCEDURE ParseModule*(VAR p: T): Ast.Branch;
VAR rv, final: Ast.Branch;
    name0, name1: Ast.Terminal;
BEGIN
   final := NIL;
   EnterCtx(p, Ast.BkModule);
   IF ~p.failed THEN
      IF AcceptOrFail(p, Lex.KMODULE) THEN
         rv := Ast.MkModule();
         name0 := ParseIdent(p);
         Ast.AddChild(rv, name0);
         IF AcceptOrFail(p, Lex.SEMI) THEN
            IF CurTok(p) = Lex.KIMPORT THEN
               Ast.AddChild(rv, ParseImportList(p))
            ELSE
               NilSlot(rv)
            END;
            Ast.AddChild(rv, ParseDeclarationSequence(p));
            IF Accept(p, Lex.KBEGIN) THEN
               Ast.AddChild(rv, ParseStatementSequence(p))
            ELSE
               NilSlot(rv)
            END;
            MustAccept(p, Lex.KEND);
            name1 := ParseIdent(p);
            IF Lex.Eql(p.scan, name0.tok, name1.tok) THEN
               IF AcceptOrFail(p, Lex.DOT) THEN
                  final := rv
               END
            ELSE
               Err(p, "END does not match module name.")
            END
         END
      END
   END;
   LeaveCtx(p);
   RETURN final
END ParseModule;


BEGIN
   (* fix up mutual recursion by hand *)
   ParseStrucType := ParseStrucTypeImpl;
   ParseProcedureBody := ParseProcedureBodyImpl;
   ParseStatementSequence := ParseStatementSequenceImpl;
   ParseExpression := ParseExpressionImpl
END Parser.
