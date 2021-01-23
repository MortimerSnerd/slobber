import 
   ast, scanner, strformat

#TODO the error handling is half assed in here.

type 
  ParseState = ref object
     src: string
     scan: Scanner

template curTok(s: ParseState) : TokenKind = s.scan.cur.kind

proc accept(ps: ParseState, tk: TokenKind) : bool = 
   if curTok(ps) == tk:
      discard next(ps.scan)
      when defined(parsedebug): echo &"TOKEN {ps.scan.cur}"
      return true
   else:
      return false

proc err(s: ParseState, msg: string) = 
   #TODO how do we report failures?
   assert false, &"@{s.scan.pos}: {msg}"

proc acceptOrFail(ps: ParseState, tk: TokenKind) : bool = 
   if accept(ps, tk):
      return true
   else:
      err(ps, &"Was expecting {tk} but got {ps.scan.cur.kind}")
      return false

proc acceptAny(ps: ParseState, tks: openarray[TokenKind]) : bool = 
   for t in tks:
      if curTok(ps) == t:
         discard next(ps.scan)
         when defined(parsedebug): echo &"TOKEN {ps.scan.cur}"
         return true
   return false

# Fwd decls
proc parseFactor(ps: ParseState) : ASTNode

let MulOperator = [ASTERISK, FSLASH, KDIV, KMOD, AMPERSAND]

# factor {MulOperator factor}
proc parseTerm(ps: ParseState) : ASTNode = 
   var f = parseFactor(ps)

   while acceptAny(ps, MulOperator):
      let op = ps.scan.prev
      f = BinaryOp(lhs: f, op: op, rhs: parseFactor(ps))

   return f

let AddOperator = [PLUS, MINUS, KOR]
# ["+"|"-"] term {AddOperator term}
proc parseSimpleExpression(ps: ParseState) : ASTNode = 
   let outer = if acceptAny(ps, [PLUS, MINUS]):
                  UnaryOp(op: ps.scan.prev)
               else:
                  nil

   var t = parseTerm(ps)
   while t != nil and acceptAny(ps, AddOperator):
      let op = ps.scan.prev
      t = BinaryOp(lhs: t, op: op, rhs: parseTerm(ps))

   if outer != nil:
      outer.rhs = t
      return outer
   else:
      return t

let Relation = [EQ, OCTOTHORPE, LT, LTE, GT, GTE, KIN, KIS]

# SimpleExpression [relation SimpleExpresion]
proc parseExpression(ps: ParseState) : ASTNode = 
   var e = parseSimpleExpression(ps)

   if acceptAny(ps, Relation):
      let op = ps.scan.prev
      e = BinaryOp(lhs: e, op: op, rhs: parseSimpleExpression(ps))

   return e

proc parseIdentDef(ps: ParseState) : IdentDef = 
   discard acceptOrFail(ps, Id)
   let rv = IdentDef(id: ps.scan.prev)

   if accept(ps, ASTERISK):
      rv.public = true

   return rv

# identdef "=" ConstExpression
proc parseConstDeclaration(ps: ParseState) : ConstDeclaration = 
   let id = parseIdentDef(ps)
   discard acceptOrFail(ps, EQ)
   return ConstDeclaration(id: id, rhs: parseExpression(ps))


proc parseSet(ps: ParseState) : SetLiteral = 
   if acceptOrFail(ps, LBRACE):
      let rv = SetLiteral(ldelim: ps.scan.prev)

      while curTok(ps) != RBRACE:
         let n = parseExpression(ps)

         if n != nil:
            if accept(ps, DOTDOT):
               let op = ps.scan.prev
               add(rv.chld, BinaryOp(op: op, lhs: n, rhs: parseExpression(ps)))
            else:
               add(rv.chld, n)

            if not accept(ps, COMMA):
               break
         else:
            err(ps, &"Was expecting expession for set value.")
            return nil

      discard acceptOrFail(ps, RBRACE)
      rv.rdelim = ps.scan.prev
      return rv
   else:
      return nil

#TODO in the BNF, INTEGER etc aren't keywords, so either make them not
# keywords, or change this to accept a standalone type keyword here.
proc parseQualIdent(ps: ParseState) : QualIdent = 
   if curTok(ps) == Id:
      let rv = QualIdent()

      while acceptOrFail(ps, Id):
         add(rv.pcs, ps.scan.prev)
         if not accept(ps, DOT):
            break
      return rv

   else:
      return nil

proc parseExpList(ps: ParseState, dest: ParentOfMany) = 
   add(dest.chld, parseExpression(ps))
   while accept(ps, COMMA):
      add(dest.chld, parseExpression(ps))

proc parseActualParameters(ps: ParseState, dest: ParentOfMany) = 
   discard acceptOrFail(ps, LPAREN)
   if curTok(ps) != RPAREN:
      parseExpList(ps, dest)
   discard acceptOrFail(ps, RPAREN)

proc parseFactor(ps: ParseState) : ASTNode = 
   if accept(ps, MINUS):
      let op = ps.scan.prev
      return UnaryOp(op: op, rhs: parseFactor(ps))
   else:
      case curTok(ps)
      of LBRACE:
         return parseSet(ps)

      of LPAREN:
         let op = ps.scan.cur
         discard next(ps.scan)
         let rv = UnaryOp(op: op, rhs: parseExpression(ps))
         discard acceptOrFail(ps, RPAREN)
         return rv

      of Id:
         let id = parseQualident(ps)

         if curTok(ps) == LPAREN: 
            let rv = Call(id: id)
            parseActualParameters(ps, rv)
            return rv
         else:
            return Var(id: id)

      else:
         if acceptAny(ps, [ConstInt, ConstReal, ConstHex, ConstHexString, 
                           ConstString, KNIL, KTRUE, KFALSE]):
            return Terminal(tok: ps.scan.prev)

proc parseType(ps: ParseState) : AstNode 
let parseConstExpression = parseExpression

# ARRAY length {"," length} OF type
# length = ConstExpression
proc parseArrayType(ps: ParseState) : AstNode = 
   discard acceptOrFail(ps, KARRAY)
   let rv = ArrayType()

   add(rv.chld, parseConstExpression(ps))
   while accept(ps, COMMA):
      add(rv.chld, parseConstExpression(ps))

   discard acceptOrFail(ps, KOF)
   add(rv.chld, parseType(ps))

   return rv

# RecordType = RECORD ["(" BaseType ")"] [FieldListSequence] END
# BaseType = qualident
# FieldListSequence = FieldList [";" FieldList]
# FieldList = IdentList ":" type
# IdentList = identdef {",', identdef}
proc parseFieldList(ps: ParseState) : FieldList = 
   let rv = FieldList()
   
   discard acceptOrFail(ps, Id)
   add(rv.chld, Terminal(tok: ps.scan.prev))
   while accept(ps, COMMA):   
      discard acceptOrFail(ps, Id)
      add(rv.chld, Terminal(tok: ps.scan.prev))

   discard acceptOrFail(ps, COLON)
   rv.ty = parseType(ps)

   return rv

proc parseRecordType(ps: ParseState) : AstNode = 
   discard acceptOrFail(ps, KRECORD)
   let rv = RecordType()

   if accept(ps, LPAREN):
      rv.base = parseQualident(ps)
      discard acceptOrFail(ps, RPAREN)

   while curTok(ps) == Id:
      add(rv.chld, parseFieldList(ps))
      while accept(ps, SEMI):
         add(rv.chld, parseFieldList(ps))

   discard acceptOrFail(ps, KEND)
   return rv

#  {ARRAY OF} qualident
proc parseFormalType(ps: ParseState) : AstNode = 
   let rv = FormalType()

   while accept(ps, KARRAY):
      discard acceptOrFail(ps, KOF)
      inc(rv.nofArrays)

   rv.id = parseQualident(ps)
   return rv

# [VAR] ident {"," ident} ":" FormalType
proc parseFPSection(ps: ParseState) : FPSection = 
   let rv = FPSection()
   
   if accept(ps, KVAR):
      rv.isVar = true
      
   discard acceptOrFail(ps, Id)
   add(rv.chld, Terminal(tok: ps.scan.prev))   
   while accept(ps, COMMA):
      discard acceptOrFail(ps, Id)
      add(rv.chld, Terminal(tok: ps.scan.prev))   

   discard acceptOrFail(ps, COLON)
   rv.ty = parseFormalType(ps)

   return rv

# "(" [FPSection {";" FPSection}] ")" [":" qualident]
proc parseFormalParameters(ps: ParseState) : FormalParameters = 
   let rv = FormalParameters()

   discard acceptOrFail(ps, LPAREN)
   
   if curTok(ps) != RPAREN:
      add(rv.chld, parseFPSection(ps))

   while accept(ps, SEMI):
      add(rv.chld, parseFPSection(ps))
   
   discard acceptOrFail(ps, RPAREN)

   if accept(ps, COLON):
      rv.retTy = parseQualident(ps)

   return rv

# PROCEDURE [FormalParameters]
proc parseProcedureType(ps: ParseState) : ProcedureType = 
   let rv = ProcedureType()

   discard acceptOrFail(ps, KPROCEDURE)
   if curTok(ps) == LPAREN:
      rv.ty = parseFormalParameters(ps)

   return rv

# qualident | ArrayType | RecordType | PointerType | ProcedureType
proc parseType(ps: ParseState) : AstNode = 
   case curTok(ps)
   of Id:
      return TypeName(id: parseQualident(ps))

   of KARRAY:
      return parseArrayType(ps)

   of KPOINTER:
      discard acceptOrFail(ps, KPOINTER)
      discard acceptOrFail(ps, KTO)
      return PointerType(rhs: parseType(ps))

   of KPROCEDURE:
      return parseProcedureType(ps)

   of KRECORD:
      return parseRecordType(ps)

   else:
      err(ps, &"Was expecing a type, got {ps.scan.cur.kind}")

# IF expression THEN StatementSequence {ELSIF expression THEN StatementSequence}
#                                      [ELSE StatementSequence] END
#proc parseStatement(ps: ParseState) : AstNode = 

# statement {";" statement}
proc parseStatementSequence(ps: ParseState) : StatementSequence = 
   let rv = StatementSequence()

   let s0 = parseStatement(ps)
   if s0 != nil:
      add(rv.chld, s0)
      while accept(ps, SEMI):
         let sn = parseStatement(ps)

         if sn != nil:
            add(rv.chld, sn)

   return rv

# [assignment | ProcedureCall | IfStatement | CaseStatement | 
#  WhileStatement | RepeatStatement | ForStatement]
proc parseStatement(ps: ParseState) : AstNode = 
   case curTok(ps)
   of KIF:
      return parseIf(ps)

   else:
      #differentiate assignment from proc call
      return nil

  
when isMainModule:
   import streams
   proc parser(txt: string) : ParseState = 
      echo "IN: " & txt
      let sc = initScanner(txt)
      return ParseState(scan: sc, src: txt)

   proc consumedAll(ps: ParseState) = 
      assert ps.scan.cur.kind == EOF, "Not all input consumed"

   var sout = newFileStream(stdout)

   let txt0 = "-Bozo.Destructo(123*x+23/2 < 100, {234, 11H..123H})"
   let ps0 = parser(txt0)
   toStr(parseFactor(ps0), sout, txt0)
   sout.write("\N")
   consumedAll(ps0)

   let txt1 = "Ass* = 12"
   let ps1 = parser(txt1)
   toStr(parseConstDeclaration(ps1), sout, txt1)
   sout.write("\N")
   consumedAll(ps1)

   let txt2 = "Jimbo.T"
   let ps2 = parser(txt2)
   toStr(parseType(ps2), sout, txt2)
   sout.write("\N")
   consumedAll(ps2)

   let txt3 = "ARRAY 1, 3 OF POINTER TO Jack.Ass"
   let ps3 = parser(txt3)
   toStr(parseType(ps3), sout, txt3)
   sout.write("\N")
   consumedAll(ps3)

   let txt4 = "RECORD(Cheese) a, b: INT; c: RECORD bones: Hoser END END"
   let ps4 = parser(txt4)
   toStr(parseType(ps4), sout, txt4)
   sout.write("\N")
   consumedAll(ps4)

   let txt5 = "RECORD hobofn: PROCEDURE(x, y: F32; VAR z: CHA) : Nards.T END"
   let ps5 = parser(txt5)
   toStr(parseType(ps5), sout, txt5)
   sout.write("\N")
   consumedAll(ps5)

