MODULE Ast;
IMPORT
   Cvt := extConvert, Dbg, Lex := Scanner, Strings;

CONST
   BranchChunkSz = 8;

   (* Branch kinds, for display, and discrimination.  We
      don't need to specialize records on the Branch type
      since we have NodeOps as a vtable anyway. (And Oberon07
      removed type bound functions). *)
   BkBranch* = 0; BkQualIdent* = 1; BkModule* = 2;BkImportList*=3;
   BkImport*=4; BkDeclarationSequence*=5; BkConstDeclaration*=6;
   BkTypeDeclaration*=7;BkArrayType*=8;BkRecordType*=9;BkFieldList*=10;
   BkPointer*=11;BkProcedureType*=12;BkFormalParameters*=13;
   BkFPSection*=14;BkFormalType*=15;BkVarDeclaration*=16;
   BkProcedureDeclaration*=17; BkProcedureBody*=18; BkBinOp*=19;
   BkUnOp*=20;BkCall*=21;BkDesignator*=22;BkSelector*=23;BkSet*=24;
   BkSetElement*=25;BkExpList*=26;BkStatementSeq*=27;BkIfStmt*=28;
   BkCaseStatement*=29;BkCase*=30;BkLabelRange*=31;BkCaseLabelList*=32;
   BkWhileStatement*=33;BkRepeatStatement*=34;BkForStatement*=35;
   BkFieldListSequence*=36;BkIdentList*=37;BkConstDeclSeq*=38;
   BkTypeDeclSeq*=39;BkVarDeclSeq*=40;BkProcDeclSeq*=41;
   BkArrayDims*=42;
   BkMax* = 64;

   (* Flags that can be set on any node *)
   NfVar*=1; (* Is a VAR param *)

   (* Indices for well known child locations for branches *)
   ModuleName* = 0; ModuleImports* = 1; ModuleDecls* = 2; ModuleInit* = 3;
   ImportAlias* = 0; ImportModule*=1;
   ConstDeclName*=0; ConstDeclVal*=1;
   TypeDeclName*=0; TypeDeclVal*=1;
   RecordBaseType*=0;RecordFieldList*=1;
   FieldListIdents*=0; FieldListType*=1;
   QualIdentModule*=0; QualIdentName*=1;
   ArrayTypeDims*=0;ArrayTypeType*=1;
   VarDeclIdents*=0;VarDeclVal*=1;
   FormalParamsReturn*=0;FormalParamsStart*=1;
   ProcedureDeclName*=0;ProcedureDeclParams*=1;ProcedureDeclBody*=2;
   ProcBodyDecls*=0; ProcBodyStmts*=1;ProcBodyReturn*=2;
   ForStmtVarName*=0;ForStmtBegin*=1;ForStmtEnd*=2;ForStmtBy*=3;ForStmtBody*=4;
   RepeatStmtBody*=0;RepeatStmtCond*=1;

   (* selKinds for MkSelector *)
   FieldAccess*=0; ArrayAccess*=1;PtrDeref*=2;TypeGuard*=3;

TYPE
   (* Root node of a tree *)
   T* = POINTER TO TreeDesc;

   NodeOps* = RECORD
      toStr*: PROCEDURE(n: T; src: ARRAY OF CHAR; indent: INTEGER)
   END;

   TreeDesc* = RECORD
      ops*: NodeOps;
      n*: INTEGER; (* used by small number of nodes *)
      flags*: SET
   END;

   Terminal* = POINTER TO TerminalDesc;
   TerminalDesc* = RECORD(T)
      tok*: Lex.Token;
      export*: BOOLEAN 
         (* valid for a terminal that's an IdentDef *)
   END;

   (* Since we can only alloc records with obnc, 
      we have to compromise how we a branch can have 
      an arbitrary number of children.  Each has an array of
      BranchChunkSz, and maintains a linked list for extensions
      if more is necesary. See AddChild() and GetChild() for details.*)
   Branch* = POINTER TO BranchDesc;
   BranchDesc* = RECORD(T)
      kind*: INTEGER;
      startIx, childLen*: INTEGER;
      chld*: ARRAY BranchChunkSz OF T;
      next, last: Branch
   END;

 VAR
   (* the joy of manual vtables *)
   defaultOps, branchOps, termOps: NodeOps;

   (* Display names for branch kinds *)
   BranchNames*: ARRAY BkMax OF ARRAY 32 OF CHAR;

PROCEDURE InitNode*(b: T); 
BEGIN
   b.n := 0;
   b.flags := {}
END InitNode;

(* Must call this to init the branch fields for any
   branch, or child of Branch *)
PROCEDURE InitBranch*(b: Branch; kind: INTEGER);
VAR i: INTEGER;
BEGIN
   IF b # NIL THEN
      InitNode(b);
      b.startIx := 0;
      b.childLen := 0;
      b.next := NIL;
      b.last := b;
      b.ops := branchOps;
      b.kind := kind;
      FOR i := 0 TO BranchChunkSz-1 DO
         b.chld[i] := NIL
      END
   END
END InitBranch;

PROCEDURE MkBranch*(): Branch;
VAR rv: Branch;
BEGIN
   NEW(rv);
   InitBranch(rv, BkBranch);
   RETURN rv
END MkBranch;

(* TODO make export member of Terminal into a Node flag *)
PROCEDURE MkTerminal*(t: Lex.Token): Terminal;
VAR rv: Terminal;
BEGIN
   NEW(rv);
   InitNode(rv);
   rv.ops := termOps;
   rv.tok := t;
   rv.export := FALSE;
   RETURN rv
END MkTerminal;

(* 0 - Module name or nil.  1 - Terminal identifier *)
PROCEDURE MkQualIdent*() : Branch;
VAR rv: Branch;
BEGIN
   NEW(rv);
   InitBranch(rv, BkQualIdent);
   RETURN rv
END MkQualIdent;

(* 0 - name terminal, 1 - import list or nil, 2 DeclSeq 
   3 - module init seq, or nil *)
PROCEDURE MkModule*(): Branch;
VAR rv: Branch;
BEGIN
   NEW(rv);
   InitBranch(rv, BkModule);
   RETURN rv
END MkModule;

PROCEDURE MkImportList*(): Branch;
VAR rv: Branch;
BEGIN
   NEW(rv);
   InitBranch(rv, BkImportList);
   RETURN rv
END MkImportList;

(* 0 - alias name or nil, 1 - module name *)
PROCEDURE MkImport*(): Branch;
VAR rv: Branch;
BEGIN
   NEW(rv);
   InitBranch(rv, BkImport);
   RETURN rv
END MkImport;

(* 0 - ConstDeclSeq, or nil.  1 - TypeDeclSeq or nil
   2 - VarDeclSeq or nil. 3 - ProcDeclSeq or nil *)
PROCEDURE MkDeclarationSequence*(): Branch;
VAR rv: Branch;
BEGIN
   NEW(rv);
   InitBranch(rv, BkDeclarationSequence);
   RETURN rv
END MkDeclarationSequence;

(* 0 - identdef, 1 - val *)
PROCEDURE MkConstDeclaration*(): Branch;
VAR rv: Branch;
BEGIN
   NEW(rv);
   InitBranch(rv, BkConstDeclaration);
   RETURN rv
END MkConstDeclaration;

(* 0 identdef, 1 - decl *)
PROCEDURE MkTypeDeclaration*(): Branch;
VAR rv: Branch;
BEGIN
   NEW(rv);
   InitBranch(rv, BkTypeDeclaration);
   RETURN rv
END MkTypeDeclaration;

(* 0 - list of dim expression, 1 contained type *)
PROCEDURE MkArrayType*(): Branch;
VAR rv: Branch;
BEGIN
   NEW(rv);
   InitBranch(rv, BkArrayType);
   RETURN rv
END MkArrayType;

(* 0 - base type, or nil.  1 - FieldListSequence *)
PROCEDURE MkRecordType*(): Branch;
VAR rv: Branch;
BEGIN
   NEW(rv);
   InitBranch(rv, BkRecordType);
   RETURN rv
END MkRecordType;

(* 0 - list of IdentDefs, 1 - some sort of type *)
PROCEDURE MkFieldList*(): Branch;
VAR rv: Branch;
BEGIN
   NEW(rv);
   InitBranch(rv, BkFieldList);
   RETURN rv
END MkFieldList;

(* 0 - contained type *)
PROCEDURE MkPointer*(): Branch;
VAR rv: Branch;
BEGIN
   NEW(rv);
   InitBranch(rv, BkPointer);
   RETURN rv
END MkPointer;

(* 0 - formal params *)
PROCEDURE MkProcedureType*(): Branch;
VAR rv: Branch;
BEGIN
   NEW(rv);
   InitBranch(rv, BkProcedureType);
   RETURN rv
END MkProcedureType;

(* 0 - return value, 1..N: N parameters *)
PROCEDURE MkFormalParameters*(): Branch;
VAR rv: Branch;
BEGIN
   NEW(rv);
   InitBranch(rv, BkFormalParameters);
   RETURN rv
END MkFormalParameters;

(* 0..N-1 - N var names, N - type.  NfVAR set if it's a ver param. *)
PROCEDURE MkFPSection*(): Branch;
VAR rv: Branch;
BEGIN
   NEW(rv);
   InitBranch(rv, BkFPSection);
   RETURN rv
END MkFPSection;

(* 0 - qualident of type.  T.n = number of ARRAY OF's *)
PROCEDURE MkFormalType*(): Branch;
VAR rv: Branch;
BEGIN
   NEW(rv);
   InitBranch(rv, BkFormalType);
   RETURN rv
END MkFormalType;


(* 0 - IdentList, 1 - type *)
PROCEDURE MkVarDeclaration*(): Branch;
VAR rv: Branch;
BEGIN
   NEW(rv);
   InitBranch(rv, BkVarDeclaration);
   RETURN rv
END MkVarDeclaration;

(* 0 - identdef name.  1 - formal parameters. 2 - body *)
PROCEDURE MkProcedureDeclaration*(): Branch;
VAR rv: Branch;
BEGIN
   NEW(rv);
   InitBranch(rv, BkProcedureDeclaration);
   RETURN rv
END MkProcedureDeclaration;

(* 0 - decl sequence.  1 - statement sequence. 2 - return expr. *)
PROCEDURE MkProcedureBody*(): Branch;
VAR rv: Branch;
BEGIN
   NEW(rv);
   InitBranch(rv, BkProcedureBody);
   RETURN rv
END MkProcedureBody;

(* 0 - lhs, 1 - op terminal, 2 - rhs *)
PROCEDURE MkBinOp*(): Branch;
VAR rv: Branch;
BEGIN
   NEW(rv);
   InitBranch(rv, BkBinOp);
   RETURN rv
END MkBinOp;

(* 0 - op, 1 - rhs *)
PROCEDURE MkUnOp*(): Branch;
VAR rv: Branch;
BEGIN
   NEW(rv);
   InitBranch(rv, BkUnOp);
   RETURN rv
END MkUnOp;

(* 0 - designator.  1 - ActualParams *)
PROCEDURE MkCall*(): Branch;
VAR rv: Branch;
BEGIN
   NEW(rv);
   InitBranch(rv, BkCall);
   RETURN rv
END MkCall;

(* 0 - qualident, 0..N selectors after *)
PROCEDURE MkDesignator*(): Branch;
VAR rv: Branch;
BEGIN
   NEW(rv);
   InitBranch(rv, BkDesignator);
   RETURN rv
END MkDesignator;

(* selKind = FieldAccess: 0 - field terminal
           = ArrayAccess: 0 - expression list
           = PtrDeref: no args
           = TypeGuard: 0 - qualident 
   selKind is kept in n.*)
PROCEDURE MkSelector*(selKind: INTEGER): Branch;
VAR rv: Branch;
BEGIN
   NEW(rv);
   InitBranch(rv, BkSelector);
   rv.n := selKind;
   RETURN rv
END MkSelector;

(* Just a list of set elements *)
PROCEDURE MkSet*(): Branch;
VAR rv: Branch;
BEGIN
   NEW(rv);
   InitBranch(rv, BkSet);
   RETURN rv
END MkSet;

(* 0 - start expr, 1 - nil, or end expression for a range *)
PROCEDURE MkSetElement*(): Branch;
VAR rv: Branch;
BEGIN
   NEW(rv);
   InitBranch(rv, BkSetElement);
   RETURN rv
END MkSetElement;

(* list of expressions *)
PROCEDURE MkExpList*(): Branch;
VAR rv: Branch;
BEGIN
   NEW(rv);
   InitBranch(rv, BkExpList);
   RETURN rv
END MkExpList;

(* List of statements *)
PROCEDURE MkStatementSeq*(): Branch;
VAR rv: Branch;
BEGIN
   NEW(rv);
   InitBranch(rv, BkStatementSeq);
   RETURN rv
END MkStatementSeq;

(* even - condition expression, or nil for final ELSE branch.
   odd - body for previous condition *)
PROCEDURE MkIfStmt*(): Branch;
VAR rv: Branch;
BEGIN
   NEW(rv);
   InitBranch(rv, BkIfStmt);
   RETURN rv
END MkIfStmt;

(* 0 - case expression. 1+ sequence of Cases *)
PROCEDURE MkCaseStatement*(): Branch;
VAR rv: Branch;
BEGIN
   NEW(rv);
   InitBranch(rv, BkCaseStatement);
   RETURN rv
END MkCaseStatement;

(* 0 - CaseLabelList 1 - body StmtSeq *)
PROCEDURE MkCase*(): Branch;
VAR rv: Branch;
BEGIN
   NEW(rv);
   InitBranch(rv, BkCase);
   RETURN rv
END MkCase;

(* 0 - constant or identifier.  1- nil, or another constant for a range *)
PROCEDURE MkLabelRange*(): Branch;
VAR rv: Branch;
BEGIN
   NEW(rv);
   InitBranch(rv, BkLabelRange);
   RETURN rv
END MkLabelRange;

(* seq of N LabelRange *)
PROCEDURE MkCaseLabelList*(): Branch;
VAR rv: Branch;
BEGIN
   NEW(rv);
   InitBranch(rv, BkCaseLabelList);
   RETURN rv
END MkCaseLabelList;

(* even conditions/ELSIF conditions.   odd bodies for previous condition *)
PROCEDURE MkWhileStatement*(): Branch;
VAR rv: Branch;
BEGIN
   NEW(rv);
   InitBranch(rv, BkWhileStatement);
   RETURN rv
END MkWhileStatement;

(* 0 - statement seq.  1 - condition expr *)
PROCEDURE MkRepeatStatement*(): Branch;
VAR rv: Branch;
BEGIN
   NEW(rv);
   InitBranch(rv, BkRepeatStatement);
   RETURN rv
END MkRepeatStatement;

(* 0 - var ident, 1 - begin expr, 2 - end expr, 
   3 nil or step expr, 4 statement sequence *)
PROCEDURE MkForStatement*(): Branch;
VAR rv: Branch;
BEGIN
   NEW(rv);
   InitBranch(rv, BkForStatement);
   RETURN rv
END MkForStatement;

(* 0..N of FieldList *)
PROCEDURE MkFieldListSequence*(): Branch;
VAR rv: Branch;
BEGIN
   NEW(rv);
   InitBranch(rv, BkFieldListSequence);
   RETURN rv
END MkFieldListSequence;

(* 0..N identdef *)
PROCEDURE MkIdentList*(): Branch;
VAR rv: Branch;
BEGIN
   NEW(rv);
   InitBranch(rv, BkIdentList);
   RETURN rv
END MkIdentList;

(* 0..N ConstDeclaration *)
PROCEDURE MkConstDeclSeq*(): Branch;
VAR rv: Branch;
BEGIN
   NEW(rv);
   InitBranch(rv, BkConstDeclSeq);
   RETURN rv
END MkConstDeclSeq;

(* 0..N TypeDeclaration *)
PROCEDURE MkTypeDeclSeq*(): Branch;
VAR rv: Branch;
BEGIN
   NEW(rv);
   InitBranch(rv, BkTypeDeclSeq);
   RETURN rv
END MkTypeDeclSeq;

(* 0..N VarDeclaration *)
PROCEDURE MkVarDeclSeq*(): Branch;
VAR rv: Branch;
BEGIN
   NEW(rv);
   InitBranch(rv, BkVarDeclSeq);
   RETURN rv
END MkVarDeclSeq;

(* 0..N ProcedureDeclaration *)
PROCEDURE MkProcDeclSeq*(): Branch;
VAR rv: Branch;
BEGIN
   NEW(rv);
   InitBranch(rv, BkProcDeclSeq);
   RETURN rv
END MkProcDeclSeq;

(* 0..N array dimensions *)
PROCEDURE MkArrayDims*(): Branch;
VAR rv: Branch;
BEGIN
   NEW(rv);
   InitBranch(rv, BkArrayDims);
   RETURN rv
END MkArrayDims;

(* Returns the Branch item i would go into, adding chunks
   to get there if necessary. *)
PROCEDURE BranchForIndex(b: Branch; i: INTEGER): Branch;
VAR end, nb: Branch;
BEGIN
   IF i = b.childLen THEN
      end := b.last
   ELSE
      end := b
   END;
   WHILE i >= (end.startIx + BranchChunkSz) DO
      IF end.next = NIL THEN
         NEW(nb);
         nb.startIx := end.startIx + BranchChunkSz;
         nb.next := NIL;
         nb.last := NIL;
         end.next := nb;
         b.last := nb;
         end := nb
      ELSE
         end := end.next
      END
   END;
   RETURN end
END BranchForIndex;


PROCEDURE AddChild*(b: Branch; c: T);
VAR nb: Branch;
BEGIN
   nb := BranchForIndex(b, b.childLen);
   INC(b.childLen);
   nb.chld[b.childLen-1-nb.startIx] := c
END AddChild;

(* Sets the given child, expanding the node if necessary *)
PROCEDURE SetChild*(b: Branch; i: INTEGER; v: T);
VAR cb: Branch;
BEGIN
   cb := BranchForIndex(b, i);
   cb.chld[i - cb.startIx] := v
END SetChild;

(* Returns the indexed child, or NIL for items not set yet. *)
PROCEDURE GetChild*(b: Branch; i: INTEGER): T;
VAR cb: Branch;
    rv: T;
BEGIN
   cb := BranchForIndex(b, i);
   rv := cb.chld[i - cb.startIx];
   RETURN rv
END GetChild;

PROCEDURE DefToStr(n: T; src: ARRAY OF CHAR; indent: INTEGER);
BEGIN
   Dbg.Ind(indent);
   Dbg.S("a node"); Dbg.Ln
END DefToStr;

PROCEDURE TermToStr(n: T; src: ARRAY OF CHAR; indent: INTEGER);
VAR t: Terminal;
BEGIN
   t := n(Terminal);
   Dbg.Ind(indent);
   Dbg.S("[Terminal "); 
   IF t.export THEN Dbg.S("*") END;
   Dbg.Slice(src, t.tok.start, t.tok.len);
   Dbg.S("]")
END TermToStr;

PROCEDURE BranchToStr(n: T; src: ARRAY OF CHAR; indent: INTEGER);
VAR ch: T;
    i: INTEGER;
    b: Branch;
BEGIN
   IF n = NIL THEN
      Dbg.Ind(indent+1); Dbg.S("NIL")
   ELSE
      b := n(Branch);
      Dbg.Ind(indent); Dbg.S("["); Dbg.S(BranchNames[b.kind]);
      IF NfVar IN n.flags THEN Dbg.S(" VAR") END;
      IF n.n # 0 THEN
         Dbg.S(" n="); Dbg.I(n.n)
      END;
      FOR i:=0 TO b.childLen-1 DO
         ch := GetChild(b, i);
         Dbg.Ln;
         IF ch = NIL THEN
            Dbg.Ind(indent+1); Dbg.S("NIL")
         ELSE
            ch.ops.toStr(ch, src, indent+1) 
         END
      END;
      Dbg.S("]")
   END
END BranchToStr;

PROCEDURE SetupBranchNames();
VAR i: INTEGER;
    nbuf: ARRAY 16 OF CHAR;
    done: BOOLEAN;
BEGIN
   FOR i := 0 TO BkMax-1 DO
      Cvt.IntToString(i, nbuf, done);
      BranchNames[i] := "?";
      Strings.Append(nbuf, BranchNames[i])
   END;
   BranchNames[BkBranch] := "Branch";
   BranchNames[BkQualIdent] := "QualIdent";
   BranchNames[BkModule] := "Module";
   BranchNames[BkImportList] := "ImportList";
   BranchNames[BkImport] := "Import";
   BranchNames[BkDeclarationSequence] := "DeclarationSequence";
   BranchNames[BkConstDeclaration] := "ConstDeclaration";
   BranchNames[BkTypeDeclaration] := "TypeDeclaration";
   BranchNames[BkArrayType] := "ArrayType";
   BranchNames[BkRecordType] := "RecordType";
   BranchNames[BkFieldList] := "FieldList";
   BranchNames[BkPointer] := "Pointer";
   BranchNames[BkProcedureType] := "ProcedureType";
   BranchNames[BkFormalParameters] := "FormalParameters";
   BranchNames[BkFPSection] := "FPSection";
   BranchNames[BkFormalType] := "FormalType";
   BranchNames[BkVarDeclaration] := "VarDeclaration";
   BranchNames[BkProcedureDeclaration] := "ProcedureDeclaration";
   BranchNames[BkProcedureBody] := "ProcedureBody";
   BranchNames[BkBinOp] := "BinOp";
   BranchNames[BkUnOp] := "UnOp";
   BranchNames[BkCall] := "Call";
   BranchNames[BkDesignator] := "Designator";
   BranchNames[BkSelector] := "Selector";
   BranchNames[BkSet] := "Set";
   BranchNames[BkSetElement] := "SetElement";
   BranchNames[BkExpList] := "ExpList";
   BranchNames[BkStatementSeq] := "StatementSeq";
   BranchNames[BkIfStmt] := "IfStmt";
   BranchNames[BkCaseStatement] := "CaseStatement";
   BranchNames[BkCase] := "Case";
   BranchNames[BkLabelRange] := "LabelRange";
   BranchNames[BkCaseLabelList] := "CaseLabelList";
   BranchNames[BkWhileStatement] := "WhileStatement";
   BranchNames[BkRepeatStatement] := "RepeatStatement";
   BranchNames[BkForStatement] := "ForStatement";
   BranchNames[BkFieldListSequence] := "FieldListSequence";
   BranchNames[BkIdentList] := "IdentList";
   BranchNames[BkConstDeclSeq] := "ConstDeclSeq";
   BranchNames[BkTypeDeclSeq] := "TypeDeclSeq";
   BranchNames[BkVarDeclSeq] := "VarDeclSeq";
   BranchNames[BkProcDeclSeq] := "ProcDeclSeq";
   BranchNames[BkArrayDims] := "ArrayDims";


END SetupBranchNames;

BEGIN
   (* fill out "vtables" for type bound operations *)
   defaultOps.toStr := DefToStr;
   termOps.toStr := TermToStr;
   branchOps.toStr := BranchToStr;

   SetupBranchNames;
END Ast.
