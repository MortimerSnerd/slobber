(* Intermediate formats, which eventually feed code 
   generation *)
MODULE Il;
IMPORT 
   Ast, Cvt:=extConvert, Dbg, Lex:=Scanner, Semcheck, St:=Symtab, SYSTEM, Target, 
   Ty:=Types;

CONST
   (* Operations for 3 address instructions *)
   NOP=0;ADD=1;LABEL=2;VARADDR=3;DEREFPTR=4;
   ADDPTR=5;LIT=6;SUB=7;MUL=8;DIVI=9;STORE=10;
   FETCH=11;BRANCH=12;BRANCHT=13;BRANCHF=14;
   TESTGT=15;TESTEQ=16;SCOPE=17;TESTLTE=18;
   NumOps=30;
   MaxNameLen=64;

   (* Flags for OpFlags *)
   ofBin=0; ofUn=1; ofNone=2;

TYPE
   Value = RECORD 
      ty: Ty.Type
         (* Effective type *)
   END;
   ValueOpt = POINTER TO Value;
   LabelOpt = POINTER TO Label;
   Label = RECORD(Value)
      id: INTEGER
   END;

   (* A scope, right now only for marking the beginning of procs,
      with "ts" set to the TypeSym for the proc *)
   ScopeOpt = POINTER TO Scope;
   Scope = RECORD(Value)
      ts: St.TypeSym
   END;
  
   ConstValue = RECORD(Value) 
      val: St.ConstVal
   END;
   ConstValueOpt = POINTER TO ConstValue;

   VarName = RECORD(Value)
      id: INTEGER;
         (* For a src var, this will be the start offset in the source.
            For a temporary, this will be < 0 *)
      ts: St.TypeSym
         (* For a var, will have the TypeSym for the var. 
            NIL for temporaries *)
   END;
   VarNameOpt = POINTER TO VarName;

   Op = RECORD
      var: VarName;
      op: INTEGER;
      lhs, rhs: ValueOpt
         (* var, constant value *)
   END;

   (* Sequence of operations, can be of an arbitrary length. *)
   OpChunk = POINTER TO OpChunkDesc;
   OpSeq = POINTER TO RECORD
      first, last: OpChunk
   END;
   OpChunkDesc = RECORD
      ops: ARRAY 32 OF Op;
      count: INTEGER;
      prev, next: OpChunk
   END;

   (* Placeholder now, contains all of the generated code for
      module *)
   ModuleCode* = POINTER TO RECORD
      ops: OpSeq
   END;

   (* State used during IL generation *)
   GenerateState* = RECORD
      mod: St.Module;
      scan: Lex.T;
      tempNum: INTEGER;
      labelNum: INTEGER;
      code: ModuleCode;
      targ: Target.T;
      curProc: St.TypeSym
   END;

VAR
   GenStmtSeqFwd: PROCEDURE(VAR gs: GenerateState; stmts: Ast.Branch);
   OpNames: ARRAY NumOps, MaxNameLen OF CHAR;
   OpFlags: ARRAY NumOps OF SET;
   DontCare: VarName;  
      (* Sentinel dest for operations that don't have a result *)

PROCEDURE MkOpSeq(): OpSeq;
VAR rv: OpSeq;
BEGIN
   NEW(rv);
   NEW(rv.first);
   rv.last := rv.first;
   rv.first.prev := NIL;
   rv.first.next := NIL;
   RETURN rv
END MkOpSeq;

PROCEDURE Push(VAR s: OpSeq; op: Op);
   (* Add op to end of sequence *)
BEGIN
   ASSERT(s.last.next = NIL);
   IF s.last.count = LEN(s.last.ops) THEN
      NEW(s.last.next);
      s.last.next.prev := s.last;
      s.last.next.count := 0;
      s.last.next.next := NIL;
      s.last := s.last.next
   END;
   s.last.ops[s.last.count] := op;
   INC(s.last.count)
END Push;

PROCEDURE MkModuleCode(): ModuleCode;
VAR rv: ModuleCode;
BEGIN
   NEW(rv);
   rv.ops := MkOpSeq();
   RETURN rv
END MkModuleCode;

PROCEDURE InitGenerateState*(VAR gs: GenerateState; mod: St.Module; 
                             scan: Lex.T; targ: Target.T);
   (* Sets up the state needed to call the Generate functions *)
BEGIN
   gs.mod := mod;
   gs.scan := scan;
   gs.targ := targ;
   gs.tempNum := -2;  (* -1 reserved for DontCare sentinel *)
   gs.labelNum := 1;
   gs.code := MkModuleCode()
END InitGenerateState;

PROCEDURE FAILIF(cond: BOOLEAN; msg: ARRAY OF CHAR);
BEGIN
   IF cond THEN
      Dbg.S("ERROR: ");
      Dbg.S(msg);
      Dbg.Ln;
      ASSERT(FALSE)
   END
END FAILIF;

PROCEDURE Tmp(VAR gs: GenerateState; ty: Ty.Type; VAR t: VarName);
   (* Generates a new temp name *)
BEGIN
   t.id := gs.tempNum; DEC(gs.tempNum);
   t.ts := NIL;
   t.ty := ty;
END Tmp;

PROCEDURE MkTmp(VAR gs: GenerateState; ty: Ty.Type): VarNameOpt;
VAR rv: VarNameOpt;
BEGIN
   NEW(rv);
   Tmp(gs, ty, rv^)
   RETURN rv
END MkTmp;

PROCEDURE MkScope(proc: St.TypeSym): ScopeOpt;
VAR rv: ScopeOpt;
BEGIN
   NEW(rv);
   IF proc # NIL THEN
      rv.ty := proc.ty
   ELSE
      rv.ty := NIL
   END;
   rv.ts := proc
   RETURN rv
END MkScope;

PROCEDURE MkLabel(VAR gs: GenerateState): LabelOpt;
VAR rv: LabelOpt;
BEGIN 
   NEW(rv);
   rv.id := gs.labelNum; INC(gs.labelNum);
   rv.ty := Ty.VoidType;
   RETURN rv
END MkLabel;

PROCEDURE bop(VAR op: Op; nm: VarName; opcode: INTEGER; lhs, rhs: ValueOpt);
   (* Init binary op *)
BEGIN
   op.var := nm;
   op.op := opcode;
   op.lhs := lhs;
   op.rhs := rhs;
END bop;

PROCEDURE pbop(VAR gs: GenerateState; nm: VarName; opcode: INTEGER; lhs, rhs: ValueOpt);
   (* Init binary op and push it to the back of the code array *)
VAR op: Op;
BEGIN
   bop(op, nm, opcode, lhs, rhs);
   Push(gs.code.ops, op)
END pbop;

PROCEDURE uop(VAR op: Op; nm: VarName; opcode: INTEGER; param: ValueOpt);
   (* Init unary op *)
BEGIN
   op.var := nm;
   op.op := opcode;
   op.lhs := param;
END uop;

PROCEDURE puop(VAR gs: GenerateState; nm: VarName; opcode: INTEGER; param: ValueOpt);
   (* Push a uop to the end of the opcodes *)
VAR op: Op;
BEGIN
   uop(op, nm, opcode, param);
   Push(gs.code.ops, op)
END puop;

PROCEDURE opSCOPE(VAR gs: GenerateState; scope: ScopeOpt);
   (* Marks the beginning of a scope *)
BEGIN
   puop(gs, DontCare, SCOPE, scope)
END opSCOPE;

PROCEDURE opLABEL(VAR gs: GenerateState; lab: LabelOpt);
   (* Makes a label at the given location *)
BEGIN
   puop(gs, DontCare, LABEL, lab);
END opLABEL;

PROCEDURE opBRANCH(VAR gs: GenerateState; lab: LabelOpt);
   (* Make unconditional branch to the given label *)
BEGIN
   puop(gs, DontCare, BRANCH, lab)
END opBRANCH;

PROCEDURE opBRANCHT(VAR gs: GenerateState; cond: ValueOpt; lab: LabelOpt);
   (* Branch if "cond" is true *)
BEGIN
   FAILIF(cond.ty.kind # Ty.KBoolean, "Condition must be BOOL");
   pbop(gs, DontCare, BRANCHT, cond, lab)
END opBRANCHT;

PROCEDURE opBRANCHF(VAR gs: GenerateState; cond: ValueOpt; lab: LabelOpt);
   (* Branch if "cond" is false *)
BEGIN
   FAILIF(cond.ty.kind # Ty.KBoolean, "Condition must be BOOL");
   pbop(gs, DontCare, BRANCHF, cond, lab)
END opBRANCHF;

PROCEDURE VarRefTerm(gs: GenerateState; term: Ast.Terminal): VarNameOpt;
   (* Returns a var ref for a unqualified terminal *)
VAR ts: St.TypeSym;
    qn: Ty.QualName;
    rv: VarNameOpt;
BEGIN
   qn.module[0] := 0X;
   Lex.Extract(gs.scan, term.tok, qn.name);
   ts := St.FindVar(gs.mod, gs.curProc.frame, qn);
   FAILIF(ts = NIL, "No var found for terminal");
   NEW(rv);
   rv.id := term.tok.start;
   rv.ts := ts;
   rv.ty := ts.ty;
   RETURN rv
END VarRefTerm;

PROCEDURE VarRef(qident: Ast.Branch): VarNameOpt;
   (* Make a VarNameOpt for a qualified identitifer *)
VAR rv: VarNameOpt;
    term: Ast.Terminal;
    note: St.TSAnnotation;
BEGIN
   NEW(rv);
   note := St.Remember(qident);
   IF note # NIL THEN
      term := Ast.TermAt(qident, Ast.QualIdentName);
      rv.id := term.tok.start;
      rv.ts := note.ts;
      rv.ty := note.ts.ty;
   ELSE
      Dbg.S("ERROR: no typesym annotation for qident"); Dbg.Ln;
      ASSERT(FALSE)
   END
   RETURN rv
END VarRef;

PROCEDURE NewIntConst(v: INTEGER): ConstValueOpt;
VAR rv: ConstValueOpt;
BEGIN
   NEW(rv);
   rv.val.kind := St.KInteger;
   rv.val.ival := v;
   rv.ty := Ty.PrimitiveType(Ty.KInteger);
   RETURN rv
END NewIntConst;

PROCEDURE PatchLastDest(VAR gs: GenerateState; vn: VarName);
VAR ch: OpChunk;
    done: BOOLEAN;
BEGIN
   ch := gs.code.ops.last;
   done := FALSE;
   WHILE ~done & (ch # NIL) DO
      IF ch.count > 0 THEN
         ch.ops[ch.count-1].var := vn;
         done := TRUE
      ELSE
         ch := ch.prev
      END
   END
END PatchLastDest;

PROCEDURE DesigConstVal(gs: GenerateState; desig: Ast.Branch): St.OptConstVal;
VAR rv: St.OptConstVal;
    qn: Ty.QualName;
    ts: St.TypeSym;
BEGIN
   IF desig.childLen = 0 THEN
      Ty.GetQualName(Ast.BranchAt(desig, 0), gs.scan, qn);
      ts := St.FindConst(gs.mod, gs.curProc.frame, qn);
      IF ts # NIL THEN
         rv := ts.val
      ELSE
         rv := NIL
      END
   ELSE
      rv := NIL
   END;
   RETURN rv
END DesigConstVal;

PROCEDURE opVARADDR(VAR gs: GenerateState; VAR dest: VarName; 
                    vname: VarNameOpt);
   (* Pushes a VARADDR onto the end of the CodeSeq, updating the
      type of "dest" to be correct *)
VAR pt: Ty.PointerType;
BEGIN
   (* keep the types consistent so we can catch errors in
      opcode generation *)
   FAILIF(vname.ty = NIL, "opVARADDR no type on vname");
   pt := Ty.MkPointerType();
   pt.ty := vname.ty;
   dest.ty := pt;
   puop(gs, dest, VARADDR, vname)
   (* TODO - should we search backwards in the current scope to see if there's
      already a VARADDR for this var? In the same scope, the address shouldn't
      change. Would it be safe to change "dest" to the matched var? *)
END opVARADDR;

PROCEDURE opADDPTR(VAR gs: GenerateState; VAR dest: VarName;
                   ptr: VarNameOpt; offset: ValueOpt);
   (* Pushes ADDPTR.  The type of "dest" must be specified by the 
      caller, because we otherwise don't have enough
      information to know how the addition effects the type. 
      (array access vs record field access) *)
BEGIN
   FAILIF(ptr.ty.kind # Ty.KPointer, "ADDPTR not a pointer");
   FAILIF(offset.ty.kind # Ty.KInteger, "Offset not INTEGER");
   FAILIF(dest.ty = NIL, "opADDPTR: Must specify destination type");
   pbop(gs, dest, ADDPTR, ptr, offset);
END opADDPTR;

PROCEDURE opFETCH(VAR gs: GenerateState; VAR dest: VarName;
                  addr: ValueOpt);
BEGIN
   FAILIF(addr.ty.kind # Ty.KPointer, "Fetch on non-pointer");
   dest.ty := addr.ty(Ty.PointerType).ty;
   puop(gs, dest, FETCH, addr)
END opFETCH;

PROCEDURE opSTORE(VAR gs: GenerateState; lhs: VarNameOpt;
                  rhs: ValueOpt);
   (* Store "rhs" into the address in "lhs" *)
BEGIN
   pbop(gs, DontCare, STORE, lhs, rhs);
END opSTORE;

PROCEDURE GenDesigAddr(VAR gs: GenerateState; desig: Ast.Branch; 
                      VAR dest: VarName);
   (* Does address calculation code for a designator.  Not 
      meant to be called for designators for constants, as those
      won't produce an address.  This will update the type
      of dest. *) 
VAR note: Ty.TypeNote;
    qident, sel: Ast.Branch;
    prevTy: Ty.Type;
    tp, tn: VarNameOpt;
    qn: Ty.QualName;
    i: INTEGER;
    fld: Ty.RecordField;
    term: Ast.Terminal;
    ts: St.TypeSym;
    rty: Ty.RecordType;
    pty: Ty.PointerType;
BEGIN
   (* TODO: lots.  right now we just handle a var. We
      don't deal with auto-derefing pointers, that's up
      to callers. *)
   qident := Ast.BranchAt(desig, Ast.DesignatorQIdent);
   note := Ty.Remember(qident);
   tp := MkTmp(gs, note.ty);  (* TODO shouldn't this be ptr to note.ty *)
   prevTy := note.ty;
   Ty.GetQualName(qident, gs.scan, qn);
   ts := St.FindConst(gs.mod, gs.curProc.frame, qn);
   ASSERT(ts = NIL);
   opVARADDR(gs, tp^, VarRef(qident));
   FOR i := 1 TO desig.childLen-1 DO
      sel := Ast.BranchAt(desig, i);
      CASE sel.n OF
      Ast.FieldAccess:
         term := Ast.TermAt(sel, 0);
         rty := prevTy(Ty.RecordType);
         fld := Ty.FindField(rty, gs.scan, term.tok); 
         (* TODO not auto-derefing pointers here *)
         gs.targ.arch.Layout(rty);
         (* Record offset changes the pointer type *)
         pty := Ty.MkPointerType();
         pty.ty := fld.ty;
         tn := MkTmp(gs, pty);
         opADDPTR(gs, tn^, tp, NewIntConst(fld.offset));
         prevTy := pty;
         tp := tn
      END
   END;
   dest.ty := tp.ty; 
   PatchLastDest(gs, dest)
END GenDesigAddr;

PROCEDURE BinOpFor(tk: INTEGER): INTEGER;
   (* Returns the 3 address opcode for the given 
      Lex.Token.kind *)
VAR rv: INTEGER;
BEGIN
   CASE tk OF
      Lex.PLUS:         rv := ADD
      |Lex.ASTERISK:    rv := MUL
      |Lex.MINUS:       rv := SUB
      |Lex.FSLASH:      rv := DIVI
      |Lex.GT:          rv := TESTGT
      |Lex.EQ:          rv := TESTEQ
      |Lex.LTE:         rv := TESTLTE
   END;
   RETURN rv
END BinOpFor;

PROCEDURE opBINOP(VAR gs: GenerateState; VAR dest: VarName; 
                  tk: INTEGER; lhs, rhs: ValueOpt);
   (* Appends the appropriate binary op for the given 
      token type, updating the type of `dest` to be
      correct *)
BEGIN
   IF Lex.tfRelational IN Lex.TokenFlags[tk] THEN
      dest.ty := Ty.PrimitiveType(Ty.KBoolean);
   ELSE
      FAILIF(~Ty.Equal(lhs.ty, rhs.ty), "BOOL op mismatch");
      FAILIF(lhs.ty = NIL, "opBINOP no type on lhs");
      dest.ty := lhs.ty
   END;
   pbop(gs, dest, BinOpFor(tk), lhs, rhs);
END opBINOP;

(* TODO - we know the parameters of functions types aren't correct
   as they are the type the user specified, but not the type we
   might actually pass it is.  (so we'd pass a record as a pointer
   to that record, whether it is var or not).  Should we fixup
   these types in semcheck, or call some sort of function here
   that encodes these differences? *)

PROCEDURE GenExpr(VAR gs: GenerateState; t: Ast.T; VAR dest: VarName);
   (* Generates opcodes for the expression, and stores
      the result to "dest" *)
VAR cv: ConstValueOpt;
    ov: St.OptConstVal;
    err: Ast.SrcError;
    br: Ast.Branch;
    lhs, rhs: VarNameOpt;
    note: Ty.TypeNote;
    op: Ast.Terminal;
BEGIN
   IF t IS Ast.Terminal THEN
      NEW(cv);
      err := St.EvalTerminal(gs.mod, gs.scan, t(Ast.Terminal),
                             cv.val);
      ASSERT(err = NIL);
      dest.ty := Ty.PrimitiveType(cv.val.kind);
      cv.ty := dest.ty;
      puop(gs, dest, LIT, cv);
   ELSE
      br := t(Ast.Branch);
      IF br.kind = Ast.BkBinOp THEN
         (* NB assignments shouldn't get here *)
         op := Ast.TermAt(br, 1);
         note := Ty.Remember(br);
         lhs := MkTmp(gs, note.ty);
         GenExpr(gs, Ast.GetChild(br, 0), lhs^);
         rhs := MkTmp(gs, note.ty);
         GenExpr(gs, Ast.GetChild(br, 2), rhs^);
         opBINOP(gs, dest, op.tok.kind, lhs, rhs);
      ELSIF br.kind = Ast.BkDesignator THEN
         ov := DesigConstVal(gs, br);
         IF ov = NIL THEN
            note := Ty.Remember(Ast.GetChild(br, br.childLen-1));
            lhs := MkTmp(gs, note.ty); (* TODO: actually, is ptr to note.ty *)
            GenDesigAddr(gs, br, lhs^);
            opFETCH(gs, dest, lhs);
         END
      ELSE
         Dbg.S("Unimplemented crap "); Dbg.I(br.kind); Dbg.Ln;
         ASSERT(FALSE)
      END
   END
END GenExpr;

PROCEDURE GenIfStmt(VAR gs: GenerateState; br: Ast.Branch);
   (* Generate the code for a chain of IF ELSIF ELSE statements *)
VAR cond: Ast.T;
    nextTest, end: LabelOpt;
    cvar: VarNameOpt;
    i: INTEGER;
BEGIN
   end := MkLabel(gs);
   nextTest := MkLabel(gs);
   FOR i := 0 TO br.childLen-1 BY 2 DO
      cond := Ast.GetChild(br, i);
      IF cond # NIL THEN   
         cvar := MkTmp(gs, NIL);
         GenExpr(gs, Ast.GetChild(br, i), cvar^);
         opBRANCHF(gs, cvar, nextTest);
         GenStmtSeqFwd(gs, Ast.BranchAt(br, i+1));
         opBRANCH(gs, end);
         opLABEL(gs, nextTest);
         nextTest := MkLabel(gs)
      ELSE
         (* else branch, the final destination *)
         GenStmtSeqFwd(gs, Ast.BranchAt(br, i+1))
      END
   END;
   opLABEL(gs, end);
END GenIfStmt;

PROCEDURE GenRepeatStmt(VAR gs: GenerateState; br: Ast.Branch);
   (* Generate code for a repeat statement *)
VAR begin: LabelOpt;
    cvar: VarNameOpt;
BEGIN
   begin := MkLabel(gs);
   opLABEL(gs, begin);
   GenStmtSeqFwd(gs, Ast.BranchAt(br, Ast.RepeatStmtBody));
   cvar := MkTmp(gs, NIL);
   GenExpr(gs, Ast.GetChild(br, Ast.RepeatStmtCond), cvar^);
   opBRANCHF(gs, cvar, begin);
END GenRepeatStmt;

PROCEDURE GenForStmt(VAR gs: GenerateState; br: Ast.Branch);
VAR cond, exit: LabelOpt;
    countAddr, to, condName, tmp0, tmp1: VarNameOpt;
    by: ConstValueOpt;
    note: Semcheck.CVNote;
    bynode: Ast.T;
BEGIN
   exit := MkLabel(gs);
   bynode := Ast.GetChild(br, Ast.ForStmtBy);
   IF bynode = NIL THEN
      by := NewIntConst(1)
   ELSE
      note := Semcheck.RememberValue(bynode);
      FAILIF(note = NIL, "No constexpr val cached");
      NEW(by); 
      by.val := note.val;
   END;
   countAddr := MkTmp(gs, NIL);
   opVARADDR(gs, countAddr^, 
             VarRefTerm(gs, Ast.TermAt(br, Ast.ForStmtVarName))); 
   tmp0 := MkTmp(gs, NIL);
   GenExpr(gs, Ast.GetChild(br, Ast.ForStmtBegin), tmp0^);
   opSTORE(gs, countAddr, tmp0);
   cond := MkLabel(gs); opLABEL(gs, cond);
   to := MkTmp(gs, NIL);
   GenExpr(gs, Ast.GetChild(br, Ast.ForStmtEnd), to^);
   condName := MkTmp(gs, NIL);
   tmp0 := MkTmp(gs, NIL);
   opFETCH(gs, tmp0^, countAddr);
   opBINOP(gs, condName^, Lex.LTE, tmp0, to);
   opBRANCHF(gs, condName, exit);
   GenStmtSeqFwd(gs, Ast.BranchAt(br, Ast.ForStmtBody));
   tmp0 := MkTmp(gs, NIL); tmp1 := MkTmp(gs, NIL);
   opFETCH(gs, tmp0^, countAddr);
   opBINOP(gs, tmp1^, Lex.PLUS, tmp0, by);
   opSTORE(gs, countAddr, tmp0);
   opBRANCH(gs, cond);
   opLABEL(gs, exit);
END GenForStmt; 

PROCEDURE GenStmt(VAR gs: GenerateState; br: Ast.Branch);
   VAR op: Ast.Terminal;
       var, rhs: VarNameOpt;
       note: Ty.TypeNote;
BEGIN
   IF br.kind = Ast.BkBinOp THEN
      op := Ast.TermAt(br, 1);
      IF op.tok.kind = Lex.COLEQ THEN
         note := Ty.Remember(Ast.GetChild(br, 2));
         rhs := MkTmp(gs, note.ty);
         GenExpr(gs, Ast.GetChild(br, 2), rhs^); 
         var := MkTmp(gs, note.ty);  GenDesigAddr(gs, Ast.BranchAt(br, 0), var^);
         opSTORE(gs, var, rhs);
      ELSE
         Dbg.S("Unexpected binop? "); Dbg.I(op.tok.kind); Dbg.Ln;
         ASSERT(FALSE)
      END
   ELSIF br.kind = Ast.BkIfStmt THEN
      GenIfStmt(gs, br)
   ELSIF br.kind = Ast.BkRepeatStatement THEN
      GenRepeatStmt(gs, br)
   ELSIF br.kind = Ast.BkForStatement THEN
      GenForStmt(gs, br)
   ELSE
      Dbg.S("Unhandled statement? "); Dbg.I(br.kind); Dbg.Ln;
      ASSERT(FALSE)
   END
END GenStmt;

PROCEDURE GenStmtSeq(VAR gs: GenerateState; stmts: Ast.Branch);
VAR i: INTEGER;
BEGIN
   IF stmts # NIL THEN
      FOR i := 0 TO stmts.childLen-1 DO
         GenStmt(gs, Ast.BranchAt(stmts, i))
      END
   END
END GenStmtSeq;

PROCEDURE GenerateProc(VAR gs: GenerateState; proc: St.TypeSym);
   (* Generate 3 address opcodes for the given procedure *)
VAR stmts: Ast.Branch;
BEGIN
   gs.curProc := proc;
   opSCOPE(gs, MkScope(proc));
   stmts := Ast.BranchAt(proc.ty(Ty.ProcType).body, Ast.ProcBodyStmts);
   GenStmtSeq(gs, stmts);
END GenerateProc;

PROCEDURE Generate*(VAR gs: GenerateState);
VAR iter: St.TSIter;
    proc: St.TypeSym;
BEGIN
   iter := St.IterAllProcs(gs.mod);
   REPEAT
      proc := iter.op.Next(iter);
      IF proc # NIL THEN
         GenerateProc(gs, proc)
      END
   UNTIL proc = NIL;
END Generate;

PROCEDURE LogVar(vn: VarName);
BEGIN
   IF vn.id < 0 THEN
      Dbg.S("t"); Dbg.I(-vn.id);
   ELSE
      Dbg.S(vn.ts.name)
   END;
END LogVar;

PROCEDURE LogVal(vo: ValueOpt);
VAR cv: ConstValueOpt;
    buf: ARRAY 20 OF CHAR;
    done: BOOLEAN;
    sopt: ScopeOpt;
BEGIN
   IF vo IS ConstValueOpt THEN
      cv := vo(ConstValueOpt);
      CASE cv.val.kind OF
      St.KByte: 
         buf[0] := CHR(cv.val.byval);
         buf[1] := 0X;
         Dbg.S("\"); Dbg.S(buf)
      |St.KInteger:
         Cvt.IntToString(cv.val.ival, buf, done);
         Dbg.S(buf)
      |St.KBoolean:
         IF cv.val.bval THEN
            Dbg.S("TRUE")
         ELSE
            Dbg.S("FALSE")
         END
      |St.KReal:
         Cvt.RealToString(cv.val.rval, buf, done);
         Dbg.S(buf);
      |St.KChar:
         buf[0] := cv.val.cval;
         buf[1] := 0X;
         Dbg.S(buf)
      |St.KSet:
         Dbg.S("SET:");
         Cvt.IntToString(SYSTEM.VAL(INTEGER, cv.val.setval), buf, done);
         Dbg.S(buf)
      END
   ELSIF vo IS VarNameOpt THEN
      LogVar(vo(VarNameOpt)^)
   ELSIF vo IS LabelOpt THEN
      Dbg.S("L");Dbg.I(vo(LabelOpt).id);
   ELSIF vo IS ScopeOpt THEN
      Dbg.S("SCOPE "); 
      sopt := vo(ScopeOpt);
      IF sopt.ts # NIL THEN
         IF sopt.ts.name[0] = 0X THEN
            Dbg.S("[MODULE INIT]")
         ELSE
            Dbg.S(sopt.ts.name)
         END
      ELSE
         Dbg.S("?")
      END
   ELSE
      Dbg.S("WTF")
   END
END LogVal;

PROCEDURE LogCode*(gs: GenerateState);
   (* Writes out generated code, for debugging *)
VAR i: INTEGER;
    ch: OpChunk;
    id: INTEGER;
BEGIN
   ch := gs.code.ops.first;
   WHILE ch # NIL DO
      FOR i := 0 TO ch.count-1 DO
         Dbg.Ln;
         IF (ch.ops[i].op = LABEL) OR (ch.ops[i].op = SCOPE) THEN
            LogVal(ch.ops[i].lhs)
         ELSE
            Dbg.PadTo(5, " ");
            id := ch.ops[i].var.id;
            IF id # DontCare.id THEN
               LogVar(ch.ops[i].var);
               Dbg.S(" := ")
            END;
            Dbg.S(OpNames[ch.ops[i].op]); Dbg.S(" ");
            LogVal(ch.ops[i].lhs);
            IF ofBin IN OpFlags[ch.ops[i].op] THEN
               Dbg.S(" "); LogVal(ch.ops[i].rhs)
            END;
            IF id # DontCare.id THEN
               Dbg.PadTo(30, " ");
               Ty.DbgSummary(ch.ops[i].var.ty)
            END
         END
      END; 
      ch := ch.next
   END;
   Dbg.Ln
END LogCode;


PROCEDURE InitTables();
VAR i: INTEGER;
   PROCEDURE opn(i: INTEGER; nm: ARRAY OF CHAR; flags: SET);
   BEGIN
      OpNames[i] := nm;
      OpFlags[i] := flags
   END opn;
BEGIN
   FOR i := 0 TO NumOps-1 DO
      OpNames[i][0] := 0X
   END;
   opn(NOP, "NOP", {ofNone});
   opn(ADD, "ADD", {ofBin});
   opn(LABEL, "LABEL", {ofUn});
   opn(VARADDR, "VARADDR", {ofUn});
   opn(DEREFPTR, "DEREFPTR", {ofUn});
   opn(LIT, "LIT", {ofUn});
   opn(ADDPTR, "ADDPTR", {ofBin});
   opn(SUB, "SUB", {ofBin});
   opn(MUL, "MUL", {ofBin});
   opn(DIVI, "DIVI", {ofBin});
   opn(STORE, "STORE", {ofBin});
   opn(FETCH, "FETCH", {ofUn});
   opn(BRANCH, "BRANCH", {ofUn});
   opn(BRANCHT, "BRANCHT", {ofBin});
   opn(BRANCHF, "BRANCHF", {ofBin});
   opn(TESTGT, "TESTGT", {ofBin});
   opn(TESTEQ, "TESTEQ", {ofBin});
   opn(SCOPE, "SCOPE", {ofUn});
   opn(TESTLTE, "TESTLTE", {ofBin});

   DontCare.id := -1;
   DontCare.ts := NIL;
   DontCare.ty := NIL;
END InitTables;

BEGIN
   InitTables();
   GenStmtSeqFwd := GenStmtSeq;
END Il.