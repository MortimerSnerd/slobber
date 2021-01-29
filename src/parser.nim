import 
   ast, scanner, streams, strformat

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

proc mustAccept(ps: ParseState, tk: TokenKind) : bool = 
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
   discard mustAccept(ps, Id)
   let rv = IdentDef(tok: ps.scan.prev)

   if accept(ps, ASTERISK):
      rv.isExport = true

   return rv

proc parseIdent(ps: ParseState) : Terminal = 
   discard mustAccept(ps, Id)
   return Terminal(tok: ps.scan.prev)

# identdef "=" ConstExpression
proc parseConstDeclaration(ps: ParseState) : ConstDeclaration = 
   let id = parseIdentDef(ps)
   discard mustAccept(ps, EQ)
   return ConstDeclaration(id: id, rhs: parseExpression(ps))


proc parseSet(ps: ParseState) : SetLiteral = 
   if mustAccept(ps, LBRACE):
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

      discard mustAccept(ps, RBRACE)
      rv.rdelim = ps.scan.prev
      return rv
   else:
      return nil

#TODO in the BNF, INTEGER etc aren't keywords, so either make them not
# keywords, or change this to accept a standalone type keyword here.
proc parseQualIdent(ps: ParseState) : QualIdent = 
   if curTok(ps) == Id:
      let rv = QualIdent()

      while mustAccept(ps, Id):
         add(rv.pcs, ps.scan.prev)
         if not accept(ps, DOT):
            break
      return rv

   else:
      return nil

proc parseExpList(ps: ParseState, dest: ASTBranch) = 
   add(dest.chld, parseExpression(ps))
   while accept(ps, COMMA):
      add(dest.chld, parseExpression(ps))

proc parseActualParameters(ps: ParseState, dest: ASTBranch) = 
   discard mustAccept(ps, LPAREN)
   if curTok(ps) != RPAREN:
      parseExpList(ps, dest)
   discard mustAccept(ps, RPAREN)

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
         discard mustAccept(ps, RPAREN)
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
   discard mustAccept(ps, KARRAY)
   let rv = ArrayType()

   add(rv.chld, parseConstExpression(ps))
   while accept(ps, COMMA):
      add(rv.chld, parseConstExpression(ps))

   discard mustAccept(ps, KOF)
   add(rv.chld, parseType(ps))

   return rv

# RecordType = RECORD ["(" BaseType ")"] [FieldListSequence] END
# BaseType = qualident
# FieldListSequence = FieldList [";" FieldList]
# FieldList = IdentList ":" type
# IdentList = identdef {",', identdef}
proc parseFieldList(ps: ParseState) : FieldList = 
   let rv = FieldList()
   
   add(rv.chld, parseIdent(ps))
   while accept(ps, COMMA):   
      add(rv.chld, parseIdent(ps))

   discard mustAccept(ps, COLON)
   rv.ty = parseType(ps)

   return rv

proc parseRecordType(ps: ParseState) : AstNode = 
   discard mustAccept(ps, KRECORD)
   let rv = RecordType()

   if accept(ps, LPAREN):
      rv.base = parseQualident(ps)
      discard mustAccept(ps, RPAREN)

   while curTok(ps) == Id:
      add(rv.chld, parseFieldList(ps))
      while accept(ps, SEMI):
         add(rv.chld, parseFieldList(ps))

   discard mustAccept(ps, KEND)
   return rv

#  {ARRAY OF} qualident
proc parseFormalType(ps: ParseState) : AstNode = 
   let rv = FormalType()

   while accept(ps, KARRAY):
      discard mustAccept(ps, KOF)
      inc(rv.nofArrays)

   rv.id = parseQualident(ps)
   return rv

# [VAR] ident {"," ident} ":" FormalType
proc parseFPSection(ps: ParseState) : FPSection = 
   let rv = FPSection()
   
   if accept(ps, KVAR):
      rv.isVar = true
      
   add(rv.chld, parseIdent(ps))   
   while accept(ps, COMMA):
      add(rv.chld, parseIdent(ps))   

   discard mustAccept(ps, COLON)
   rv.ty = parseFormalType(ps)

   return rv

# "(" [FPSection {";" FPSection}] ")" [":" qualident]
proc parseFormalParameters(ps: ParseState) : FormalParameters = 
   let rv = FormalParameters()

   discard mustAccept(ps, LPAREN)
   
   if curTok(ps) != RPAREN:
      add(rv.chld, parseFPSection(ps))

   while accept(ps, SEMI):
      add(rv.chld, parseFPSection(ps))
   
   discard mustAccept(ps, RPAREN)

   if accept(ps, COLON):
      rv.retTy = parseQualident(ps)

   return rv

# PROCEDURE [FormalParameters]
proc parseProcedureType(ps: ParseState) : ProcedureType = 
   let rv = ProcedureType()

   discard mustAccept(ps, KPROCEDURE)
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
      discard mustAccept(ps, KPOINTER)
      discard mustAccept(ps, KTO)
      return PointerType(rhs: parseType(ps))

   of KPROCEDURE:
      return parseProcedureType(ps)

   of KRECORD:
      return parseRecordType(ps)

   else:
      err(ps, &"Was expecing a type, got {ps.scan.cur.kind}")

proc parseStatement(ps: ParseState) : AstNode  
proc parseStatementSequence(ps: ParseState) : StatementSequence

# IF expression THEN StatementSequence {ELSIF expression THEN StatementSequence}
#                                      [ELSE StatementSequence] END
proc parseIf(ps: ParseState) : IfStatement = 
   let rv = IfStatement()

   discard mustAccept(ps, KIF)
   add(rv.tests, parseExpression(ps))
   discard mustAccept(ps, KTHEN)
   add(rv.bodies, parseStatementSequence(ps))

   while accept(ps, KELSIF):
      add(rv.tests, parseExpression(ps))
      discard mustAccept(ps, KTHEN)
      add(rv.bodies, parseStatementSequence(ps))

   if accept(ps, KELSE):
      add(rv.tests, nil)
      add(rv.bodies, parseStatementSequence(ps))

   discard mustAccept(ps, KEND)
   return rv


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

# ExpList := expression {"," expression}
proc parseExpressionList(ps: ParseState) : ExpressionList = 
   let rv = ExpressionList()

   add(rv.chld, parseExpression(ps))
   while accept(ps, COMMA):
      add(rv.chld, parseExpression(ps))

   return rv

# selector := "." ident | "[" ExpList "]" | "^" |  "(" qualident ")"
# Return null if we're not at a selector.
proc maybeParseSelector(ps: ParseState) : Selector = 
   let start = restartPoint(ps.scan)

   if accept(ps, DOT):
      return Selector(kind: skFieldAccess, arg: parseIdent(ps))
   elif accept(ps, LBRACKET):
      let rv = Selector(kind: skArrayAccess, arg: parseExpressionList(ps))
      discard mustAccept(ps, RBRACKET)
      return rv
   elif accept(ps, CARET):
      return Selector(kind: skPtrDeref)
   elif accept(ps, LPAREN):
      ## We do some speculative parsing here.  There's an ambiguity
      ## with A(B) being a procedure call or a type guard.  We're not
      ## building a symbol table while we build the AST, so all we can
      ## do right now is back out of the parse if it's definitely not 
      ## a type guard.  Later on, the semcheck stage will have enough 
      ## information to fix up any mistaken one parameter function calls
      ## back to being procedure calls.
      let rv = Selector(kind: skTypeAssert, arg: parseQualIdent(ps))

      if rv.arg == nil or not accept(ps, RPAREN):
         rewind(ps.scan, start)
         return nil
      else:
         return rv
   else:
      return nil


# designator := qualident {selector}
proc parseDesignator(ps: ParseState) : Designator = 
   let rv = Designator(leading: parseQualident(ps))

   while (let sel = maybeParseSelector(ps); sel != nil):
      add(rv.chld, sel)

   return rv

# WHILE expression DO StatementSequence
# {ELSIF expresion DO StatementSequence} END
proc parseWhile(ps: ParseState) : WhileStatement = 
   let rv = WhileStatement()

   discard mustAccept(ps, KWHILE)
   add(rv.chld, parseExpression(ps))
   discard mustAccept(ps, KDO)
   add(rv.chld, parseStatementSequence(ps))

   while accept(ps, KELSIF):
      add(rv.chld, parseExpression(ps))
      discard mustAccept(ps, KDO)
      add(rv.chld, parseStatementSequence(ps))

   discard mustAccept(ps, KEND)
   return rv


# label := integer | string | qualident
let CaseLabelFirsts = [ ConstInt, ConstString, ConstHexString, Id ]
proc parseLabel(ps: ParseState) : AstNode = 
   case curTok(ps)
   of ConstInt, ConstString, ConstHexString:
      discard next(ps.scan)
      return Terminal(tok: ps.scan.prev)

   of Id:
      return parseQualident(ps)

   else:
      err(ps, "Was expecing int, string or qualfied identifier.")
      return nil
      
# LabelRange := label [".." label]
proc parseLabelRange(ps: ParseState) : LabelRange = 
   let rv = LabelRange()

   add(rv.chld, parseLabel(ps))
   if accept(ps, DOTDOT):
      add(rv.chld, parseLabel(ps))

   return rv

# CaseLabelList := LabelRange {"," LabelRange}
proc parseCaseLabelList(ps: ParseState) : CaseLabelList = 
   let rv = CaseLabelList()

   add(rv.chld, parseLabelRange(ps))
   while accept(ps, COMMA):
      add(rv.chld, parseLabelRange(ps))

   return rv
      
# case := [CaseLabelList ":" StatementSequence]
proc parseCase(ps: ParseState) : Case = 
   let rv = Case()

   if curTok(ps) in CaseLabelFirsts:
      add(rv.chld, parseCaseLabelList(ps))
      discard mustAccept(ps, COLON)
      add(rv.chld, parseStatementSequence(ps))

   return rv

# CASE expression OF case {"|" case} END
proc parseCaseStatement(ps: ParseState) : CaseStatement = 
   discard mustAccept(ps, KCASE)
   let rv = CaseStatement()

   add(rv.chld, parseExpression(ps))
   discard mustAccept(ps, KOF)
   add(rv.chld, parseCase(ps))

   while accept(ps, BAR):
      add(rv.chld, parseCase(ps))

   discard mustAccept(ps, KEND)
   return rv

# REPEAT StatementSequence UNTIL expression
proc parseRepeat(ps: ParseState) : RepeatStatement = 
   discard mustAccept(ps, KREPEAT)
   let rv = RepeatStatement()

   add(rv.chld, parseStatementSequence(ps))
   discard mustAccept(ps, KUNTIL)
   add(rv.chld, parseExpression(ps))

   return rv

# FOR ident ":=" expression TO expresion [BY ConstExpression]
#    DO StatementSequence END
proc parseFor(ps: ParseState) : ForStatement = 
   discard mustAccept(ps, KFOR)
   let rv = ForStatement()

   add(rv.chld, parseIdent(ps))
   discard mustAccept(ps, COLEQ)
   add(rv.chld, parseExpression(ps))
   discard mustAccept(ps, KTO)
   add(rv.chld, parseExpression(ps))
   if accept(ps, KBY):
      add(rv.chld, parseExpression(ps))
   else:
      add(rv.chld, nil)

   discard mustAccept(ps, KDO)
   add(rv.chld, parseStatementSequence(ps))
   discard mustAccept(ps, KEND)

   return rv

# [assignment | ProcedureCall | IfStatement | CaseStatement | 
#  WhileStatement | RepeatStatement | ForStatement]
proc parseStatement(ps: ParseState) : AstNode = 
   case curTok(ps)
   of KIF:
      return parseIf(ps)

   of KWHILE:
      return parseWhile(ps)

   of KCASE:
      return parseCaseStatement(ps)

   of KREPEAT:
      return parseRepeat(ps)

   of KFOR:
      return parseFor(ps)

   of Id:
      # Both ProcedureCall and Assignment have a leading production
      # of "designator".  This is not fully resolved at parse time, so 
      # there might be a skTypeGuard selector for a single arg parameter
      # call.
      let d = parseDesignator(ps)

      if accept(ps, COLEQ):
         let rv = Assignment()
         add(rv.chld, d)
         add(rv.chld, parseExpression(ps))
         return rv
      elif accept(ps, LPAREN):
         let rv = ProcedureCall()
         add(rv.chld, d)
         add(rv.chld, parseExpressionList(ps))
         discard mustAccept(ps, RPAREN)
         return rv
      elif d.endsWithTypeAssert():  
         # Just a type assert by itself is ok.  It might also be a function 
         # that doesn't get fixed up till symcheck.
         return d
      else:
         err(ps, &"Was expecting statement, got {ps.scan.cur.kind}.")
         return nil

   else:
      return nil

proc parseDeclarationSequence(ps: ParseState) : DeclarationSequence

# ProcedureHeading := PROCEDURE identdef [FormalParameters]
proc parseProcedureHeading(ps: ParseState) : ProcedureHeading = 
   discard mustAccept(ps, KPROCEDURE)
   let rv = ProcedureHeading()

   add(rv.chld, parseIdentDef(ps))
   add(rv.chld, if curTok(ps) == LPAREN: parseFormalParameters(ps) else: nil)
   return rv

# ProcedureBody := DeclarationSequence [BEGIN StatementSequence]
#                  [RETURN expression] END
proc parseProcedureBody(ps: ParseState) : ProcedureBody =
   let rv = ProcedureBody()

   add(rv.chld, parseDeclarationSequence(ps))
   add(rv.chld, if accept(ps, KBEGIN): parseStatementSequence(ps) else: nil)
   add(rv.chld, if accept(ps, KRETURN): parseExpression(ps) else: nil)
   discard mustAccept(ps, KEND)
   return rv

# ProcedureDeclaration := ProcedureHeading ";" ProcedureBody ident
proc parseProcedureDeclaration(ps: ParseState) : ProcedureDeclaration = 
   let rv = ProcedureDeclaration()

   add(rv.chld, parseProcedureHeading(ps))
   discard mustAccept(ps, SEMI)
   add(rv.chld, parseProcedureBody(ps))
   add(rv.chld, parseIdent(ps))
   return rv


# identdef = type
proc parseTypeDeclaration(ps: ParseState) : TypeDeclaration = 
   let rv = TypeDeclaration()

   add(rv.chld, parseIdentDef(ps))
   discard mustAccept(ps, EQ)
   add(rv.chld, parseType(ps))
   return rv

# IdentList ":" type
proc parseVariableDeclaration(ps: ParseState) : FieldList = 
   return parseFieldList(ps)

# DeclarationSequence := [CONST {ConstDeclaration ";"}]
#                        [TYPE {TypeDeclaration ";"}]
#                        [VAR {VariableDeclaration ";"}]
#                        {ProcedureDeclaration ";"}
proc parseDeclarationSequence(ps: ParseState) : DeclarationSequence = 
   let rv = DeclarationSequence()

   if accept(ps, KCONST):
      while curTok(ps) == Id:
         add(rv.chld, parseConstDeclaration(ps))
         discard mustAccept(ps, SEMI)

   if accept(ps, KTYPE):
      while curTok(ps) == Id:
         add(rv.chld, parseTypeDeclaration(ps))
         discard mustAccept(ps, SEMI)

   if accept(ps, KVAR):
      while curTok(ps) == Id:
         add(rv.chld, parseVariableDeclaration(ps))
         discard mustAccept(ps, SEMI)

   while curTok(ps) == KPROCEDURE:
      add(rv.chld, parseProcedureDeclaration(ps))
      discard mustAccept(ps, SEMI)

   return rv


   
#import := ident [":=" ident].
proc parseImport(ps: ParseState) : Import = 
   let rv = Import()
   add(rv.chld, parseIdent(ps))
   if accept(ps, COLEQ):
      # Alias goes in slot 1.
      add(rv.chld, rv.chld[0])
      rv.chld[0] = parseIdent(ps)

   return rv

#ImportList := IMPORT import {"," import} ";".
proc parseImportList(ps: ParseState) : ImportList = 
   discard mustAccept(ps, KIMPORT)
   let rv = ImportList()
   add(rv.chld, parseImport(ps))
   while accept(ps, COMMA):
      add(rv.chld, parseImport(ps))

#module := MODULE ident ";" [ImportList] DeclarationSequence
#         [BEGIN StatementSequence] END ident "." .
proc parseModule(ps: ParseState) : Module = 
   discard mustAccept(ps, KMODULE)
   let rv = Module()
   add(rv.chld, parseIdent(ps))
   discard mustAccept(ps, SEMI)
   add(rv.chld, if curTok(ps) == KIMPORT: parseImportList(ps) else: nil)
   add(rv.chld, parseDeclarationSequence(ps))
   add(rv.chld, if accept(ps, KBEGIN): parseStatementSequence(ps) else: nil)
   discard mustAccept(ps, KEND)
   add(rv.chld, parseIdent(ps))
   discard mustAccept(ps, DOT)
   return rv

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

   let txt6 = "IF bozo > 1+2 THEN ELSIF cheese THEN ELSE END"
   let ps6 = parser(txt6)
   toStr(parseStatement(ps6), sout, txt6)
   sout.write("\N")
   consumedAll(ps6)

   let txt7 = "Moddy.bim[12,x].hoser^"
   let ps7 = parser(txt7)
   toStr(parseDesignator(ps7), sout, txt7)
   sout.write("\N")
   consumedAll(ps7)

   let txt8 = "Dick(t).bo := 12; Dick.Tater(d) := \"barf\"; Jimmy.Cheesburger(12)"
   let ps8 = parser(txt8)
   toStr(parseStatementSequence(ps8), sout, txt8)
   sout.write("\N")
   consumedAll(ps8)

   let txt9 = "WHILE x < 12 DO x := x + 1 ELSIF x < 0 DO x := 1 END"
   let ps9 = parser(txt9)
   toStr(parseStatementSequence(ps9), sout, txt9)
   sout.write("\N")
   consumedAll(ps9)

   let txt10 = "CASE x+3 OF 1: print(x) | 2..10, 15: print(99) END"
   let ps10 = parser(txt10)
   toStr(parseStatementSequence(ps10), sout, txt10)
   sout.write("\N")
   consumedAll(ps10)

   let txt11 = "REPEAT IO.print(JackAss); x := x+1 UNTIL x = 12"
   let ps11 = parser(txt11)
   toStr(parseStatementSequence(ps11), sout, txt11)
   sout.write("\N")
   consumedAll(ps11)

   let txt12 = "FOR x := 1 TO 10 DO Check.This(x, 12) END"
   let ps12 = parser(txt12)
   toStr(parseStatementSequence(ps12), sout, txt12)
   sout.write("\N")
   consumedAll(ps12)

   let txt13 = "FOR x := 1 TO 10 BY 2 DO Check.This(x, 13) END"
   let ps13 = parser(txt13)
   toStr(parseStatementSequence(ps13), sout, txt13)
   sout.write("\N")
   consumedAll(ps13)

   let txt15 = """
   PROCEDURE Default*(VAR o: R);
   CONST balls = 123;
   VAR up: Ast.Declarations;
       i: Momo;
   BEGIN
      o.checkIndex := TRUE;
      o.checkArith := TRUE;
      RETURN o
   END Default"""
   let ps15 = parser(txt15)
   toStr(parseProcedureDeclaration(ps15), sout, txt15)
   sout.write("\N")
   consumedAll(ps15)

   let txt16 = """
      MODULE GeneratorC;
      IMPORT
         V, Ast, SpecIdent := OberonSpecIdent
      CONST
         Supported* = TRUE;
         Implementation=0;
      TYPE
         PMemoryOut = POINTER TO MemoryOut;
         MemoryOut = RECORD(Stream.Out)
            mem: ARRAY 2 OF RECORD
               buf: ARRAY 4096 of CHAR;
               len: Inty
            END;
            invert: BOOLO
         END;
      VAR
         hoser = Supported;
      PROCEDURE MemoryWrite(VAR out: MemoryOut; buf: ARRAY OF CHAR; ofs, count: INO);
      BEGIN
         Ass()
      END MemoryWrite;
   END GeneratorC.
"""
   let ps16 = parser(txt16)
   toStr(parseModule(ps16), sout, txt16)
   sout.write("\N")
   consumedAll(ps16)

