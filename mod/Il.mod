(* Intermediate formats, which eventually feed code 
   generation *)
MODULE Il;
IMPORT 
   Ast, Cvt:=extConvert, Dbg, Lex:=Scanner, Segment, Semcheck, St:=Symtab, 
   Strings, SYSTEM, Target, Ty:=Types;

CONST
   (* Operations for 3 address instructions *)
   NOP=0;ADD=1;LABEL=2;VARADDR=3;DEREFPTR=4;
   ADDPTR=5;LIT=6;SUB=7;MUL=8;DIVI=9;STORE=10;
   FETCH=11;BRANCH=12;BRANCHT=13;BRANCHF=14;
   TESTGT=15;TESTEQ=16;SCOPE=17;TESTLTE=18;
   CALL=19;CALLPARAM=20;RET=21;TESTLT=22;
   BAND=23; PARAMADDR=24;NEGATE=25;BOR=26;
   TESTNEQ=27;ASSIGN=28;TESTGTE=29;CAST=30;
   ESAD=31;TESTIN=32;ICALL=33;
   NumOps=34;
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
      var: VarNameOpt;
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
      curProc: St.TypeSym;
      isTestCall: VarNameOpt
         (* Name used to call RUNTIME.ISTEST *)
   END;

VAR
   GenExprFwd: PROCEDURE(VAR gs: GenerateState; t: Ast.T): VarNameOpt;
   GenStmtSeqFwd: PROCEDURE(VAR gs: GenerateState; stmts: Ast.Branch);
      (* Forward decls to break recursion *)

   OpNames: ARRAY NumOps, MaxNameLen OF CHAR;
   OpFlags: ARRAY NumOps OF SET;
   DontCare: VarNameOpt;  
      (* Sentinel dest for operations that don't have a result *)
   LogCodeFwd: PROCEDURE(gs: GenerateState);

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

PROCEDURE ClearProcScope(VAR gs: GenerateState);
   (* Clears the scope state in preparation for
      starting a new proc scope *)
BEGIN
END ClearProcScope;

PROCEDURE InitGenerateState*(VAR gs: GenerateState; mod: St.Module; 
                             scan: Lex.T; targ: Target.T);
   (* Sets up the state needed to call the Generate functions *)
BEGIN
   gs.mod := mod;
   gs.scan := scan;
   gs.targ := targ;
   gs.tempNum := -2;  (* -1 reserved for DontCare sentinel *)
   gs.labelNum := 1;
   gs.code := MkModuleCode();
   gs.isTestCall := NIL
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

PROCEDURE NewIntConst(v: INTEGER): ConstValueOpt;
VAR rv: ConstValueOpt;
BEGIN
   NEW(rv);
   rv.val.kind := St.KInteger;
   rv.val.ival := v;
   rv.ty := Ty.PrimitiveType(Ty.KInteger);
   RETURN rv
END NewIntConst;

PROCEDURE NewBoolConst(v: BOOLEAN): ConstValueOpt;
VAR rv: ConstValueOpt;
BEGIN
   NEW(rv);
   rv.val.kind := St.KBoolean;
   rv.val.bval := v;
   rv.ty := Ty.PrimitiveType(Ty.KBoolean);
   RETURN rv
END NewBoolConst;

PROCEDURE bop(VAR op: Op; nm: VarNameOpt; opcode: INTEGER; lhs, rhs: ValueOpt);
   (* Init binary op *)
BEGIN
   op.var := nm;
   op.op := opcode;
   op.lhs := lhs;
   op.rhs := rhs;
END bop;

PROCEDURE pbop(VAR gs: GenerateState; nm: VarNameOpt; opcode: INTEGER; lhs, rhs: ValueOpt);
   (* Init binary op and push it to the back of the code array *)
VAR op: Op;
BEGIN
   bop(op, nm, opcode, lhs, rhs);
   Push(gs.code.ops, op)
END pbop;

PROCEDURE uop(VAR op: Op; nm: VarNameOpt; opcode: INTEGER; param: ValueOpt);
   (* Init unary op *)
BEGIN
   op.var := nm;
   op.op := opcode;
   op.lhs := param;
END uop;

PROCEDURE puop(VAR gs: GenerateState; nm: VarNameOpt; opcode: INTEGER; param: ValueOpt);
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

PROCEDURE opESAD(VAR gs: GenerateState; esadCode: INTEGER);
   (* Generates an abort statement *)
BEGIN
   puop(gs, DontCare, ESAD, NewIntConst(esadCode))
END opESAD;

PROCEDURE opCALLPARAM(VAR gs: GenerateState; val: ValueOpt);
BEGIN
   puop(gs, DontCare, CALLPARAM, val)
END opCALLPARAM;

PROCEDURE opICALL(VAR gs: GenerateState; numParams: INTEGER; 
                 call: VarNameOpt): VarNameOpt;
   (* Writes a indirect call op, returning the var that contains the
      result.  (even if there's no return type) *)
VAR rv: VarNameOpt;
    pt: Ty.ProcType;
    retTy: Ty.Type;
BEGIN
   pt := call.ty(Ty.ProcType);
   IF pt.returnTy = NIL THEN
      retTy := Ty.VoidType
   ELSE
      retTy := pt.returnTy
   END;
   rv := MkTmp(gs, retTy);
   pbop(gs, rv, ICALL, NewIntConst(numParams), call);
   RETURN rv
END opICALL;

PROCEDURE opCALL(VAR gs: GenerateState; numParams: INTEGER; 
                 call: VarNameOpt): VarNameOpt;
   (* Writes a call op, returning the var that contains the
      result.  (even if there's no return type *)
VAR rv: VarNameOpt;
    pt: Ty.ProcType;
    retTy: Ty.Type;
BEGIN
   pt := call.ty(Ty.ProcType);
   IF pt.returnTy = NIL THEN
      retTy := Ty.VoidType
   ELSE
      retTy := pt.returnTy
   END;
   rv := MkTmp(gs, retTy);
   pbop(gs, rv, CALL, NewIntConst(numParams), call);
   RETURN rv
END opCALL;

PROCEDURE opRET(VAR gs: GenerateState; returnVal: VarNameOpt);
   (* Returns a value from a function.  "returnVal" can be
      DontCare for proper procedures *)
BEGIN
   puop(gs, DontCare, RET, returnVal)
END opRET;


PROCEDURE opBRANCHF(VAR gs: GenerateState; cond: ValueOpt; lab: LabelOpt);
   (* Branch if "cond" is false *)
BEGIN
   FAILIF(cond.ty.kind # Ty.KBoolean, "Condition must be BOOL");
   pbop(gs, DontCare, BRANCHF, cond, lab)
END opBRANCHF;

PROCEDURE VarForTypeSym(ts: St.TypeSym): VarNameOpt;
   (* Return a var name for a TypeSym *)
VAR rv: VarNameOpt;
BEGIN
   NEW(rv);
   rv.id := 1;
   rv.ts := ts;
   rv.ty := ts.ty;
   RETURN rv
END VarForTypeSym;

PROCEDURE VarRefTerm(gs: GenerateState; term: Ast.Terminal): VarNameOpt;
   (* Returns a var ref for a unqualified terminal *)
VAR ts: St.TypeSym;
    qn: Ty.QualName;
BEGIN
   qn.module[0] := 0X;
   Lex.Extract(gs.scan, term.tok, qn.name);
   ts := St.FindVar(gs.mod, gs.curProc.frame, qn);
   FAILIF(ts = NIL, "No var found for terminal");
   RETURN VarForTypeSym(ts)
END VarRefTerm;

PROCEDURE VarRef(gs: GenerateState; qident: Ast.Branch): VarNameOpt;
   (* Make a VarNameOpt for a qualified identitifer *)
VAR rv: VarNameOpt;
    term: Ast.Terminal;
    note: St.TSAnnotation;
BEGIN
   NEW(rv);
   note := St.Remember(qident);
   IF note # NIL THEN
      term := Ast.TermAt(qident, Ast.QualIdentName);
      rv.id := 1;
      rv.ts := note.ts;
      rv.ty := note.ts.ty;
   ELSE
      Dbg.S("ERROR: no typesym annotation for qident"); Dbg.Ln;
      qident.ops.toStr(qident, gs.scan.buf, 0); Dbg.Ln;
      ASSERT(FALSE)
   END
   RETURN rv
END VarRef;

PROCEDURE ProcParamFor(gs: GenerateState; qident: Ast.Branch): Ty.ProcParam;
   (* If qident is a procedure param, returns the ProcParam for it.
      Returns NIL otherwise. *)
VAR rv: Ty.ProcParam;
    qn: Ty.QualName;
    ts: St.TypeSym;
BEGIN
   Ty.GetQualName(qident, gs.scan, qn);
   ts := St.FindVar(gs.mod, gs.curProc.frame, qn);
   IF (ts # NIL) & (ts.kind = St.tsProcParam) THEN
      rv := Ty.FindParam(gs.curProc.ty(Ty.ProcType), qn.name); 
      ASSERT(rv # NIL)  (* should always be found, unless scoping rules have
                           changed *)
   ELSE
      rv := NIL
   END;
   RETURN rv
END ProcParamFor;

PROCEDURE DesigConstVal(gs: GenerateState; desig: Ast.Branch): St.OptConstVal;
VAR rv: St.OptConstVal;
    qn: Ty.QualName;
    ts: St.TypeSym;
BEGIN
   IF desig.childLen = 1 THEN
      Ty.GetQualName(Ast.BranchAt(desig, 0), gs.scan, qn);
      ts := St.FindConst(gs.mod, gs.curProc.frame, qn);
      IF ts # NIL THEN
         rv := ts.val;
         ASSERT(rv # NIL)
      ELSE
         rv := NIL
      END
   ELSE
      rv := NIL
   END;
   RETURN rv
END DesigConstVal;

PROCEDURE opPARAMADDR(VAR gs: GenerateState; paramTy: Ty.Type;
                      paramNo: INTEGER): VarNameOpt;
   (* Gets the var address for the given parameter number for the function. 
      Native codegen will need to treat this op specially for the VAR
      parameter case. *)
VAR rv: VarNameOpt;
BEGIN
   rv := MkTmp(gs, Ty.MkPointerTo(paramTy));  (* leak *)
   puop(gs, rv, PARAMADDR, NewIntConst(paramNo));
   RETURN rv
END opPARAMADDR;   

PROCEDURE opVARADDR(VAR gs: GenerateState; vname: VarNameOpt): VarNameOpt;
   (* Pushes a VARADDR onto the end of the CodeSeq, returning a new
      tmp var that holds the result *)
VAR pt: Ty.PointerType;
    dest: VarNameOpt;
BEGIN
   (* keep the types consistent so we can catch errors in
      opcode generation *)
   FAILIF(vname.ty = NIL, "opVARADDR no type on vname");
   pt := Ty.MkPointerType();
   pt.ty := vname.ty;
   dest := MkTmp(gs, pt);
   puop(gs, dest, VARADDR, vname);
   RETURN dest
END opVARADDR;

PROCEDURE opADDPTR(VAR gs: GenerateState; destType: Ty.Type;
                   ptr: VarNameOpt; offset: ValueOpt): VarNameOpt;
   (* Pushes ADDPTR.  The type of return value must be specified by the 
      caller, because we otherwise don't have enough
      information to know how the addition effects the type. 
      (array access vs record field access) *)
VAR dest: VarNameOpt;
BEGIN
   FAILIF(ptr.ty.kind # Ty.KPointer, "ADDPTR not a pointer");
   FAILIF(offset.ty.kind # Ty.KInteger, "Offset not INTEGER");
   dest := MkTmp(gs, destType);
   pbop(gs, dest, ADDPTR, ptr, offset);
   RETURN dest
END opADDPTR;

PROCEDURE opFETCH(VAR gs: GenerateState; addr: ValueOpt): VarNameOpt;
   (* Adds a fetch operation, and returns the temp var it was
      fetched into *)
VAR dest: VarNameOpt;
BEGIN
   FAILIF(addr.ty.kind # Ty.KPointer, "Fetch on non-pointer");
   dest := MkTmp(gs, addr.ty(Ty.PointerType).ty);
   puop(gs, dest, FETCH, addr);
   RETURN dest
END opFETCH;

PROCEDURE opCAST(VAR gs: GenerateState; destType: Ty.Type;
                 val: ValueOpt): VarNameOpt;
   (* Casts the of "val" to "destType" and returns the resulting var *)
VAR rv: VarNameOpt;
BEGIN
   rv := MkTmp(gs, destType);
   puop(gs, rv, CAST, val);
   RETURN rv
END opCAST;

PROCEDURE opSTORE(VAR gs: GenerateState; lhs: VarNameOpt;
                  rhs: ValueOpt);
   (* Store "rhs" into the address in "lhs" *)
BEGIN
   pbop(gs, DontCare, STORE, lhs, rhs);
END opSTORE;

PROCEDURE BinOpFor(tk: INTEGER): INTEGER;
   (* Returns the 3 address opcode for the given 
      Lex.Token.kind *)
VAR rv: INTEGER;
BEGIN
   Dbg.I(tk);Dbg.Ln;
   CASE tk OF
      Lex.PLUS:         rv := ADD
      |Lex.ASTERISK:    rv := MUL
      |Lex.MINUS:       rv := SUB
      |Lex.FSLASH:      rv := DIVI
      |Lex.GT:          rv := TESTGT
      |Lex.GTE:         rv := TESTGTE
      |Lex.EQ:          rv := TESTEQ
      |Lex.LTE:         rv := TESTLTE
      |Lex.LT:          rv := TESTLT
      |Lex.AMPERSAND:   rv := BAND
      |Lex.KOR:         rv := BOR
      |Lex.OCTOTHORPE:  rv := TESTNEQ
      |Lex.KIN:         rv := TESTIN
   END;
   RETURN rv
END BinOpFor;

PROCEDURE opBINOP(VAR gs: GenerateState; tk: INTEGER; 
                  lhs, rhs: ValueOpt): VarNameOpt;
   (* Appends the appropriate binary op for the given 
      token type, returning the tempory var the result
      is stored in. *)
VAR dest: VarNameOpt;
BEGIN
   IF Lex.tfRelational IN Lex.TokenFlags[tk] THEN
      dest := MkTmp(gs,  Ty.PrimitiveType(Ty.KBoolean));
   ELSE
      FAILIF(~Ty.Equal(lhs.ty, rhs.ty), "op mismatch");
      FAILIF(lhs.ty = NIL, "opBINOP no type on lhs");
      dest := MkTmp(gs, lhs.ty);
   END;
   pbop(gs, dest, BinOpFor(tk), lhs, rhs);
   RETURN dest
END opBINOP;

PROCEDURE opASSIGN(VAR gs: GenerateState; lhs: VarNameOpt;
                   rhs: ValueOpt);
   (* Assigns a new value to a var. Use sparingly *)
BEGIN
   ASSERT(Ty.Equal(lhs.ty, rhs.ty));
   pbop(gs, DontCare, ASSIGN, lhs, rhs)
END opASSIGN;

PROCEDURE opLIT(VAR gs: GenerateState; val: ValueOpt): VarNameOpt;
   (* Make a new temp var with the given value and return the var *)
VAR rv: VarNameOpt;
BEGIN
   rv := MkTmp(gs, val.ty);
   puop(gs, rv, LIT, val);
   RETURN rv
END opLIT;

PROCEDURE opUNOP(VAR gs: GenerateState; op: INTEGER; 
                 rhs: ValueOpt): VarNameOpt;
   (* Add unary operation and return the result *)
VAR rv: VarNameOpt;
BEGIN
   rv := MkTmp(gs, rhs.ty);
   puop(gs, rv, op, rhs);
   RETURN rv
END opUNOP;

PROCEDURE GenFieldAccess(VAR gs: GenerateState; rty: Ty.RecordType; 
                         fld: Ty.RecordField; 
                         recordAddr: VarNameOpt): VarNameOpt;
   (* Generates the code that calulates the address of 
      field "fld" from record pointer "recordAddr".
      Returns the tmpvar the value is stored in. *)
VAR pty: Ty.PointerType;
    offset:INTEGER;
BEGIN
   (* TODO not auto-derefing pointers here *)
   gs.targ.arch.Layout(rty);
   (* Record offset changes the pointer type *)
   pty := Ty.MkPointerType();
   pty.ty := fld.ty;
   (* Adjust the offset for the implicit vtable pointer *)
   offset := fld.offset + gs.targ.arch.PointerSize;
   RETURN opADDPTR(gs, pty, recordAddr, NewIntConst(offset))
END GenFieldAccess;


PROCEDURE IsTestCall(VAR gs: GenerateState): VarNameOpt;
VAR proc: St.TypeSym;
    qn: Ty.QualName;
BEGIN
   IF gs.isTestCall = NIL THEN
      qn.module := "RUNTIME";
      qn.name := "ISTEST";
      proc := St.FindProc(gs.mod, gs.curProc.frame, qn);
      NEW(gs.isTestCall);
      gs.isTestCall.id := 1;
      gs.isTestCall.ts := proc;
      gs.isTestCall.ty := proc.ty;
   END;
   RETURN gs.isTestCall
END IsTestCall;

PROCEDURE VTableVarName(sym: St.TypeSym; VAR dest: Ty.QualName);
   (* Populates dest with the name of the vtable var for 
      sym. Sym can be a record, or pointer to a record *)
VAR ty: Ty.Type;
BEGIN
   ty := Ty.DerefPointers(sym.ty);
   dest := ty.srcName^;
   Strings.Append("_VT", dest.name);
END VTableVarName;

PROCEDURE GenIsTestVar(VAR gs: GenerateState; lhs: VarNameOpt; 
                       typeName: Ty.QualName): VarNameOpt;
   (* Helper for GenIsTest that takes a var and a qualified
      name *)
VAR rhs: St.TypeSym;
    vtabName: Ty.QualName;
BEGIN
   opCALLPARAM(gs, lhs);
   rhs := St.FindType(gs.mod, gs.curProc.frame, typeName);
   vtabName := typeName;
   VTableVarName(rhs, vtabName);
   rhs := St.FindVar(gs.mod, gs.curProc.frame, vtabName);
   opCALLPARAM(gs, opVARADDR(gs, VarForTypeSym(rhs)));
   RETURN opCALL(gs, 2, IsTestCall(gs))
END GenIsTestVar;

PROCEDURE GenIsTest(VAR gs: GenerateState; br: Ast.Branch): VarNameOpt;
VAR lhs: VarNameOpt;
    qn: Ty.QualName;
BEGIN
   lhs := GenExprFwd(gs, Ast.GetChild(br, 0));
   Ty.GetQualName(Ast.BranchAt(Ast.BranchAt(br, 2), 0), gs.scan, qn);
   RETURN GenIsTestVar(gs, lhs, qn)
END GenIsTest;

PROCEDURE GenTypeGuard(VAR gs: GenerateState; sel: Ast.Branch; 
                       val: VarNameOpt): VarNameOpt;
   (* Generates type guard code, returning the variable the successful
      cast would be in *)
VAR check: VarNameOpt;
    qn: Ty.QualName;
    targetTy: St.TypeSym;
    pass: LabelOpt;
BEGIN
   FAILIF(~Ty.IsRecordRef(val.ty(Ty.PointerType).ty), "Only records can be cast");
   Ty.GetQualName(Ast.BranchAt(sel, 0), gs.scan, qn);
   pass := MkLabel(gs);
   check := GenIsTestVar(gs, val, qn);
   opBRANCHT(gs, check, pass);
   opESAD(gs, Ast.esadFailedGuard);
   opLABEL(gs, pass);
   targetTy := St.FindType(gs.mod, gs.curProc.frame, qn);
   RETURN opCAST(gs, Ty.MkPointerTo(targetTy.ty), val)
END GenTypeGuard;

PROCEDURE GenDesigAddr(VAR gs: GenerateState; desig: Ast.Branch): VarNameOpt; 
   (* Does address calculation code for a designator.  Not 
      meant to be called for designators for constants, as those
      won't produce an address.  Returns a var name for the result. *) 
VAR note: Ty.TypeNote;
    qident, sel: Ast.Branch;
    prevTy: Ty.Type;
    tp, tn, t0, t1: VarNameOpt;
    qn: Ty.QualName;
    i, sz, align: INTEGER;
    fld: Ty.RecordField;
    term: Ast.Terminal;
    ts: St.TypeSym;
    rty: Ty.RecordType;
    pty: Ty.PointerType;
    arty: Ty.ArrayType;
    param: Ty.ProcParam;
BEGIN
   (* TODO: lots.  right now we just handle a var. We
      don't deal with auto-derefing pointers, that's up
      to callers. *)
   (* TODO really need to auto-deref pointers for record field access.  
           Can take out manual derefs from RUNTIME.ISTEST as a test case *)
   qident := Ast.BranchAt(desig, Ast.DesignatorQIdent);
   note := Ty.Remember(qident);
   Ty.GetQualName(qident, gs.scan, qn);
   ts := St.FindConst(gs.mod, gs.curProc.frame, qn);
   ASSERT(ts = NIL);
   param := ProcParamFor(gs, qident);
   IF param # NIL THEN
      tp := opPARAMADDR(gs, param.ty, param.ord)
   ELSE
      tp := opVARADDR(gs, VarRef(gs, qident));
   END;
   prevTy := tp.ty;
   FOR i := 1 TO desig.childLen-1 DO
      sel := Ast.BranchAt(desig, i);
      CASE sel.n OF
      Ast.FieldAccess:
         pty := prevTy(Ty.PointerType);
         IF pty.ty.kind = Ty.KPointer THEN
            (* Dereference one level of pointers for lhs
               of field access *)
            pty := pty.ty(Ty.PointerType);
            tp := opFETCH(gs, tp);
            prevTy := pty;
         END;
         rty := pty.ty(Ty.RecordType);
         term := Ast.TermAt(sel, 0);
         fld := Ty.FindField(rty, gs.scan, term.tok); 
         tp := GenFieldAccess(gs, rty, fld, tp);
         prevTy := tp.ty;
      |Ast.ArrayAccess:
         arty := prevTy(Ty.PointerType).ty(Ty.ArrayType);
         sz := gs.targ.arch.SizeOf(arty.ty);
         align := gs.targ.arch.Alignment(arty.ty);
         sz := Target.Align(sz, align);
         t0 := GenExprFwd(gs, Ast.GetChild(sel, 0)); 
         t1 := opBINOP(gs, Lex.ASTERISK, t0, NewIntConst(sz));
         pty := Ty.MkPointerType();
         pty.ty := arty.ty;
         tn := opADDPTR(gs, pty, tp, t1);
         prevTy := pty;
         tp := tn
      |Ast.PtrDeref:
         pty := prevTy(Ty.PointerType);
         tn := MkTmp(gs, NIL);
         tp := opFETCH(gs, tp);
         prevTy := pty.ty
      |Ast.TypeGuard:
         tp := GenTypeGuard(gs, sel, tp);
         prevTy := tp.ty  
      END
   END;
   RETURN tp
END GenDesigAddr;

PROCEDURE GetVTable(VAR gs: GenerateState; recordPtr: VarNameOpt): VarNameOpt;
   (* Generates the code to fetch the vtable pointer for the given record. 
      Implements RUNTIME.VTABLEFOR *)
VAR t0: VarNameOpt;
    vtab: St.TypeSym;
    qn: Ty.QualName;
BEGIN
   (* The VTable pointer is the first field in the record *)
   IF Ast.StringEq(gs.mod.name, "RUNTIME") THEN
      qn.module := ""
   ELSE
      qn.module := "RUNTIME";
   END;
   qn.name := "VTABLEPTRPTR";
   vtab := St.FindType(gs.mod, gs.curProc.frame, qn);
   t0 := opADDPTR(gs, vtab.ty, recordPtr, NewIntConst(0));
   RETURN opFETCH(gs, t0)
END GetVTable;

PROCEDURE SpeciallyHandledCall(VAR gs: GenerateState; 
                               desig, params: Ast.Branch;
                               proc: St.TypeSym): VarNameOpt;
   (* If desig is call that needs custom code generation,
      handle it and return the result var.  Returns NIL
      otherwise *)
VAR rv, t0: VarNameOpt;
    tn: Ty.TypeNote;
    param: Ty.ProcParam;
    paramDesig: Ast.Branch;
BEGIN
   rv := NIL;
   IF St.BelongsToModule(proc, "Globals") 
         & Ast.StringEq(proc.name, "LEN") THEN
      tn := Ty.Remember(Ast.GetChild(params, 0));
      IF Ty.OpenArray IN tn.ty.flags THEN
         (* Currently, this only happens for parameters, so find the proc
            param and get its neighbor *)
          paramDesig := Ast.BranchAt(params, 0);
          ASSERT(paramDesig.childLen = 1); (* Just the param, not an open array if there's a field ref *)
          param := ProcParamFor(gs, Ast.BranchAt(paramDesig, 0));
          (* Len is the next parameter after the open array *)
          t0 := opPARAMADDR(gs, Ty.PrimitiveType(Ty.KInteger), param.ord+1);
          rv := opFETCH(gs, t0); 
      ELSE
         (* Answer is in the type *)
         rv := MkTmp(gs, Ty.PrimitiveType(Ty.KInteger));
         puop(gs, rv, LIT,
               NewIntConst(tn.ty(Ty.ArrayType).dim));
      END
   ELSIF St.BelongsToModule(proc, "RUNTIME") THEN
      IF Ast.StringEq(proc.name, "VTABLEFOR") THEN
         (* Convert record pointer to vtable pointer *)
         t0 := GenExprFwd(gs, Ast.GetChild(params, 0));
         rv := GetVTable(gs, t0);
      ELSIF Ast.StringEq(proc.name, "HALT") THEN
         t0 := GenExprFwd(gs, Ast.GetChild(params, 0));
         puop(gs, DontCare, ESAD, t0);
         rv := DontCare;
      END;
   ELSIF St.BelongsToModule(proc, "SYSTEM") THEN
      IF Ast.StringEq(proc.name, "VAL") THEN
         tn := Ty.Remember(Ast.GetChild(params, 0));
         t0 := GenExprFwd(gs, Ast.GetChild(params, 1));
         rv := opCAST(gs, tn.ty, t0)
      END;
   END;
   RETURN rv
END SpeciallyHandledCall;  

PROCEDURE GenCall(VAR gs: GenerateState; call: Ast.Branch): VarNameOpt;
   (* Generates a function call sequence.  If there's a return 
      value, returns the tmp var it will be in.  Otherwise, returns
      NIL *)
VAR param: Ty.ProcParam;
    proc: Ty.ProcType;
    pt: St.TSAnnotation;
    params, desig: Ast.Branch;
    i, paramCount: INTEGER;
    vty: Ty.TypeNote;
    rv, callopt, pexpr, ptrpcall, pcall: VarNameOpt;
    tmp: Ast.T;
BEGIN
   (* NB Calls can be nested, so when the code generator sees
         CALLPARAM instructions, it should put them on a stack, 
         and when it hits a CALL it can pop of the correct number
         for the called procedure *)
   (* TODO not passing for VAR parameters correctly, should be passing
      the address, not the value of the var *)
   desig := Ast.BranchAt(call, Ast.CallDesignator);
   (* desig.ops.toStr(desig, gs.scan.buf, 0); Dbg.Ln; *)
   params := Ast.BranchAt(call, Ast.CallParams);
   pt := St.Remember(Ast.GetChild(desig, desig.childLen-1));
   IF pt # NIL THEN
      pcall := NIL;
      proc := pt.ts.ty(Ty.ProcType);
   ELSE
      (* Designator is a variable that holds a proc ptr *)
      ptrpcall := GenDesigAddr(gs, desig);
      pcall := opFETCH(gs, ptrpcall);
      vty := Ty.Remember(Ast.GetChild(desig, desig.childLen-1));
      proc := vty.ty(Ty.ProcType)
   END;
   IF pt # NIL THEN
      rv := SpeciallyHandledCall(gs, desig, params, pt.ts);
   ELSE
      rv := NIL
   END;
   IF rv = NIL THEN
      param := proc.params;
      paramCount := 0;
      IF params # NIL THEN
         FOR i := 0 TO params.childLen-1 DO
            IF param.isVar THEN
               (* For a var parameter, we have to be passing in a 
                  var designator, and we only want to pass in the 
                  address of the var *)
               tmp := Ast.GetChild(params, i);
               FAILIF(~(tmp IS Ast.Branch), "VAR param not branch");
               FAILIF(tmp(Ast.Branch).kind # Ast.BkDesignator, "VAR param not designator");
               pexpr := GenDesigAddr(gs, tmp(Ast.Branch))
            ELSE
               pexpr := GenExprFwd(gs, Ast.GetChild(params, i));
            END;
            IF param.ty.kind = Ty.KArray THEN
               (* Open array calling convention - breaks it into
                  two parameters, the pointer to the array and the 
                  length of the array *)
                  IF pexpr.ty.kind # Ty.KPointer THEN
                     LogCodeFwd(gs)
                  END;
                  FAILIF(pexpr.ty.kind # Ty.KPointer, "array param not ptr");
                  opCALLPARAM(gs, pexpr);
                  opCALLPARAM(gs, NewIntConst(
                     pexpr.ty(Ty.PointerType).ty(Ty.ArrayType).dim));
                  paramCount := paramCount + 2;
            ELSE
                  opCALLPARAM(gs, pexpr);
                  INC(paramCount)
            END;
            param := param.next;
         END;
      END;
      IF pt # NIL THEN
         NEW(callopt);
         callopt.id := 1;
         callopt.ts := pt.ts;
         callopt.ty := pt.ts.ty;
         rv := opCALL(gs, paramCount, callopt)
      ELSE
         rv := opICALL(gs, paramCount, pcall)
      END;
   END;
   RETURN rv
END GenCall;

PROCEDURE FitsInAVar(ty: Ty.Type): BOOLEAN;
   (* If this is true, we can load this type of
      value into a temp var, otherwise it's some sort
      of composite type that we just need to keep a 
      pointer to *)
   RETURN (ty.kind = Ty.KPointer) 
          OR ((ty.kind # Ty.KAny) & Ty.IsPrimitive(ty))
END FitsInAVar;

PROCEDURE PtrTargetFitsInAVar(ty: Ty.Type): BOOLEAN;
   (* Returns true if ty is a pointer and it points
      to a value that fits in a var *)
   RETURN (ty.kind = Ty.KPointer) & FitsInAVar(ty(Ty.PointerType).ty)
END PtrTargetFitsInAVar;

PROCEDURE GenShortCircuitBool(VAR gs: GenerateState; tok: Lex.TokKind; 
                              br: Ast.Branch): VarNameOpt;
   (* Make code for short-circuit evaluation of OR or & *)
VAR rv, ex: VarNameOpt;
    end: LabelOpt;
BEGIN
   end := MkLabel(gs);
   rv := GenExprFwd(gs, Ast.GetChild(br, 0));
   CASE tok OF
   Lex.KOR: 
      opBRANCHT(gs, rv, end);
      ex := GenExprFwd(gs, Ast.GetChild(br, 2));
      opASSIGN(gs, rv, ex);
      opLABEL(gs, end)
   |Lex.AMPERSAND: 
      opBRANCHF(gs, rv, end);
      ex := GenExprFwd(gs, Ast.GetChild(br, 2));
      opASSIGN(gs, rv, ex);
      opLABEL(gs, end)
   END;
   RETURN rv
END GenShortCircuitBool;

PROCEDURE EvalSetLiteral(VAR gs: GenerateState; set: Ast.Branch): ConstValueOpt;
   (* Evalate a set literal and returns a ConstValueOpt for it *)
VAR rv: ConstValueOpt;
    elem: Ast.Branch;
    startb, endb: Ast.T;
    i, j: INTEGER;
    err: Ast.SrcError;
    startVal, endVal: St.ConstVal;
BEGIN
   NEW(rv);
   rv.val.kind := St.KSet;
   rv.val.setval := {};
   rv.val.ival := 0;
   rv.ty := St.TypeOf(rv.val);
   FOR i := 0 TO set.childLen-1 DO
      elem := Ast.BranchAt(set, i);
      startb := Ast.GetChild(elem, 0);
      endb := Ast.GetChild(elem, 1);
      err := St.EvalConstExpr(gs.mod, gs.curProc.frame, gs.scan, startb,
                              startVal);
      FAILIF(err # NIL, "set literal constexpr");
      FAILIF(startVal.kind # St.KInteger, "set elem not int");
      IF endb = NIL THEN
         endVal := startVal
      ELSE
         err := St.EvalConstExpr(gs.mod, gs.curProc.frame, gs.scan, 
                                 endb, endVal);
         FAILIF(err # NIL, "set end elem literal constexpr");
         FAILIF(endVal.kind # St.KInteger, "set end elem not int");
      END;
      FOR j := startVal.ival TO endVal.ival DO
         INCL(rv.val.setval, j)
      END;
   END;
   RETURN rv
END EvalSetLiteral;

(* TODO - we know the parameters of functions types aren't correct
   as they are the type the user specified, but not the type we
   might actually pass it is.  (so we'd pass a record as a pointer
   to that record, whether it is var or not).  Should we fixup
   these types in semcheck, or call some sort of function here
   that encodes these differences? *)

PROCEDURE GenExpr(VAR gs: GenerateState; t: Ast.T): VarNameOpt;
   (* Generates opcodes for the expression, and returns the tmp
      var the result was stored in *)
VAR cv: ConstValueOpt;
    ov: St.OptConstVal;
    err: Ast.SrcError;
    br: Ast.Branch;
    lhs, rhs: VarNameOpt;
    note: Ty.TypeNote;
    op: Ast.Terminal;
    dest: VarNameOpt;
BEGIN
   IF t IS Ast.Terminal THEN
      NEW(cv);
      err := St.EvalTerminal(gs.mod, gs.scan, t(Ast.Terminal),
                             cv.val);
      ASSERT(err = NIL);
      dest := MkTmp(gs, St.TypeOf(cv.val));
      IF ~FitsInAVar(dest.ty) THEN
         (* string (or future record) literal *)
         dest.ty := Ty.MkPointerTo(dest.ty)
      END;
      cv.ty := dest.ty;
      puop(gs, dest, LIT, cv);
   ELSE
      br := t(Ast.Branch);
      IF br.kind = Ast.BkBinOp THEN
         (* NB assignments shouldn't get here *)
         op := Ast.TermAt(br, 1);
         IF op.tok.kind = Lex.KIS THEN
            dest := GenIsTest(gs, br)
         ELSIF (op.tok.kind = Lex.KOR) OR (op.tok.kind = Lex.AMPERSAND) THEN
            dest := GenShortCircuitBool(gs, op.tok.kind, br)
         ELSE
            note := Ty.Remember(br);
            lhs := GenExpr(gs, Ast.GetChild(br, 0));
            rhs := GenExpr(gs, Ast.GetChild(br, 2));
            dest := opBINOP(gs, op.tok.kind, lhs, rhs);
         END
      ELSIF br.kind = Ast.BkDesignator THEN
         ov := DesigConstVal(gs, br);
         IF ov = NIL THEN
            note := Ty.Remember(Ast.GetChild(br, br.childLen-1));
            lhs := GenDesigAddr(gs, br);
            (* Don't fetch non-primitive types into variables, just handle
               their pointers *)
            IF PtrTargetFitsInAVar(lhs.ty) THEN
               dest := opFETCH(gs, lhs);
            ELSE
               dest := lhs
            END
         ELSE
            NEW(cv);
            cv.val := ov^;
            cv.ty := St.TypeOf(cv.val);
            dest := opLIT(gs, cv)
         END
      ELSIF br.kind = Ast.BkCall THEN
         dest := GenCall(gs, br)
      ELSIF br.kind = Ast.BkParenExpr THEN
         dest := GenExpr(gs, Ast.GetChild(br, 0))
      ELSIF br.kind = Ast.BkUnOp THEN
         op := Ast.TermAt(br, 0);
         rhs := GenExpr(gs, Ast.GetChild(br, 1));
         CASE op.tok.kind OF
         Lex.MINUS, Lex.TILDE:  
            dest := opUNOP(gs, NEGATE, rhs)
         END
      ELSIF br.kind = Ast.BkSet THEN
         dest := opLIT(gs, EvalSetLiteral(gs, br))
      ELSE
         Dbg.S("Unimplemented crap "); Dbg.I(br.kind); Dbg.Ln;
         ASSERT(FALSE)
      END
   END
   RETURN dest
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
         cvar := GenExpr(gs, Ast.GetChild(br, i));
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

PROCEDURE GenWhileStmt(VAR gs: GenerateState; stmt: Ast.Branch);
VAR labStart, labNextTest: LabelOpt;
    tBody: Ast.Branch;
    tCond: Ast.T;
    vCond: VarNameOpt;
    i: INTEGER;
BEGIN
   labStart := MkLabel(gs);
   labNextTest := NIL;
   opLABEL(gs, labStart);
   FOR i := 0 TO stmt.childLen-1 BY 2 DO
      tCond := Ast.BranchAt(stmt, i);
      tBody := Ast.BranchAt(stmt, i+1);
      IF labNextTest # NIL THEN
         opLABEL(gs, labNextTest)
      END;
      labNextTest := MkLabel(gs);
      vCond := GenExpr(gs, tCond);
      opBRANCHF(gs, vCond, labNextTest);
      GenStmtSeqFwd(gs, tBody);
      opBRANCH(gs, labStart);
   END;
   IF labNextTest # NIL THEN
      opLABEL(gs, labNextTest)
   END;
END GenWhileStmt;

PROCEDURE GenRepeatStmt(VAR gs: GenerateState; br: Ast.Branch);
   (* Generate code for a repeat statement *)
VAR begin: LabelOpt;
    cvar: VarNameOpt;
BEGIN
   begin := MkLabel(gs);
   opLABEL(gs, begin);
   GenStmtSeqFwd(gs, Ast.BranchAt(br, Ast.RepeatStmtBody));
   cvar := GenExpr(gs, Ast.GetChild(br, Ast.RepeatStmtCond));
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
      by.ty := St.TypeOf(note.val);
   END;
   countAddr := opVARADDR(gs, 
      VarRefTerm(gs, Ast.TermAt(br, Ast.ForStmtVarName))); 
   tmp0 := GenExpr(gs, Ast.GetChild(br, Ast.ForStmtBegin));
   opSTORE(gs, countAddr, tmp0);
   cond := MkLabel(gs); opLABEL(gs, cond);
   to := GenExpr(gs, Ast.GetChild(br, Ast.ForStmtEnd));
   tmp0 := opFETCH(gs, countAddr);
   condName := opBINOP(gs, Lex.LTE, tmp0, to);
   opBRANCHF(gs, condName, exit);
   GenStmtSeqFwd(gs, Ast.BranchAt(br, Ast.ForStmtBody));
   tmp0 := opFETCH(gs, countAddr);
   tmp1 := opBINOP(gs, Lex.PLUS, tmp0, by);
   opSTORE(gs, countAddr, tmp0);
   opBRANCH(gs, cond);
   opLABEL(gs, exit);
END GenForStmt; 

PROCEDURE GenStmt(VAR gs: GenerateState; br: Ast.Branch);
VAR op: Ast.Terminal;
    var, rhs: VarNameOpt;
    note: Ty.TypeNote;
    i: INTEGER;
BEGIN
   IF br.kind = Ast.BkBinOp THEN
      op := Ast.TermAt(br, 1);
      IF op.tok.kind = Lex.COLEQ THEN
         note := Ty.Remember(Ast.GetChild(br, 2));
         rhs := GenExpr(gs, Ast.GetChild(br, 2)); 
         var := GenDesigAddr(gs, Ast.BranchAt(br, 0));
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
   ELSIF br.kind = Ast.BkCall THEN
      rhs := GenCall(gs, br)
   ELSIF br.kind = Ast.BkWhileStatement THEN
      GenWhileStmt(gs, br)
   ELSIF br.kind = Ast.BkStatementSeq THEN
      FOR i := 0 TO br.childLen-1 DO
         GenStmt(gs, Ast.BranchAt(br, i))
      END
   ELSE
      Dbg.S("Unhandled statement? "); Dbg.I(br.kind); Dbg.Ln;
      br.ops.toStr(br, gs.scan.buf, 0); Dbg.Ln;
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
VAR body, stmts: Ast.Branch;
    ret: Ast.T;
BEGIN
   ClearProcScope(gs);
   gs.curProc := proc;
   opSCOPE(gs, MkScope(proc));
   body := proc.ty(Ty.ProcType).body;
   stmts := Ast.BranchAt(body, Ast.ProcBodyStmts);
   GenStmtSeq(gs, stmts);
   ret := Ast.GetChild(body, Ast.ProcBodyReturn);
   IF ret = NIL THEN
      opRET(gs, DontCare);
   ELSE
      opRET(gs, GenExpr(gs, ret))
   END      
END GenerateProc;

PROCEDURE Generate*(VAR gs: GenerateState);
VAR iter: St.TSIter;
    proc: St.TypeSym;
BEGIN
   (* TODO Generate unique type ids.  the code that generates 
           actual machine code and the data sections will need to 
           do this.  If we cheese it and link our own format into
           a ELF exe as a dirty first pass, then the linking stage
           could take care of that *)
   iter := St.IterAllProcs(gs.mod);
   REPEAT
      proc := iter.op.Next(iter);
      IF proc # NIL THEN
         GenerateProc(gs, proc)
      END
   UNTIL proc = NIL;
END Generate;

PROCEDURE LogVar(vn: VarNameOpt);
BEGIN
   IF vn.id < 0 THEN
      IF vn.id = DontCare.id THEN
         Dbg.S("DontCare")
      ELSE
         Dbg.S("t"); Dbg.I(-vn.id);
      END
   ELSE
      IF vn.ts.frame # NIL THEN
         Dbg.S(vn.ts.frame.owningModule.name);
         Dbg.S(".")
      END;
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
      St.KNil:
         Dbg.S("NIL")
      |St.KByte: 
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
      |St.KString:
         Dbg.S("[string]")
      END
   ELSIF vo IS VarNameOpt THEN
      LogVar(vo(VarNameOpt))
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
    param: Ty.ProcParam;
BEGIN
   ch := gs.code.ops.first;
   WHILE ch # NIL DO
      FOR i := 0 TO ch.count-1 DO
         Dbg.Ln;
         IF ch.ops[i].op = LABEL THEN
            LogVal(ch.ops[i].lhs)
         ELSIF ch.ops[i].op = SCOPE THEN
            Dbg.Ln;
            LogVal(ch.ops[i].lhs);
            param := ch.ops[i].lhs(ScopeOpt).ts.ty(Ty.ProcType).params;
            IF param # NIL THEN
               Dbg.S("(");
               WHILE param # NIL DO
                  Dbg.I(param.ord);
                  Dbg.S(":"); Dbg.S(param.name);
                  IF param.next # NIL THEN Dbg.S(", ") END;
                  param := param.next
               END;
               Dbg.S(")");
            END
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
               Dbg.PadTo(40, " ");
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
   opn(BAND, "BAND", {ofBin});
   opn(STORE, "STORE", {ofBin});
   opn(FETCH, "FETCH", {ofUn});
   opn(BRANCH, "BRANCH", {ofUn});
   opn(BRANCHT, "BRANCHT", {ofBin});
   opn(BRANCHF, "BRANCHF", {ofBin});
   opn(TESTGT, "TESTGT", {ofBin});
   opn(TESTEQ, "TESTEQ", {ofBin});
   opn(SCOPE, "SCOPE", {ofUn});
   opn(TESTLTE, "TESTLTE", {ofBin});
   opn(TESTLT, "TESTLT", {ofBin});
   opn(CALL, "CALL", {ofBin});
   opn(ICALL, "ICALL", {ofBin});
   opn(CALLPARAM, "CALLPARAM", {ofUn});
   opn(RET, "RET", {ofUn});
   opn(PARAMADDR, "PARAMADDR", {ofUn});
   opn(NEGATE, "NEGATE", {ofUn});
   opn(BOR, "BOR", {ofBin});
   opn(TESTNEQ, "TESTNEQ", {ofBin});
   opn(ASSIGN, "ASSIGN", {ofBin});
   opn(TESTGTE, "TESTGTE", {ofBin});
   opn(CAST, "CAST", {ofUn});
   opn(ESAD, "ESAD", {ofUn});
   opn(TESTIN, "TESTIN", {ofBin});

   NEW(DontCare);
   DontCare.id := -1;
   DontCare.ts := NIL;
   DontCare.ty := NIL;
END InitTables;

BEGIN
   InitTables();
   GenStmtSeqFwd := GenStmtSeq;
   GenExprFwd := GenExpr;
   LogCodeFwd := LogCode;
END Il.
