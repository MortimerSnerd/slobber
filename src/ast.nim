import 
   scanner, streams

type
   AstNode* = ref object of RootRef
      discard

   ConstDeclaration* = ref object of AstNode
      id*: IdentDef
      rhs*: AstNode

   ASTBranch* = ref object of AstNode
      chld*: seq[ASTNode]

   Terminal* = ref object of AstNode
      ## Id or a literal.
      tok*: Token

   IdentDef* = ref object of Terminal
      isExport*: bool

   QualIdent* = ref object of AstNode
      pcs*: seq[Token]

   UnaryOp* = ref object of AstNode
     op*: Token
     rhs*: AstNode

   BinaryOp* = ref object of AstNode
     op*: Token
     lhs*: AstNode
     rhs*: AstNode

   ActualParameters* = ref object of ASTBranch
   SetLiteral* = ref object of ASTBranch
      ldelim*: Token
      rdelim*: Token

   Call* = ref object of ASTBranch
      id*: QualIdent

   Var* = ref object of AstNode
      id*: QualIdent

   TypeName* = ref object of AstNode
      id*: QualIdent

   ArrayType* = ref object of ASTBranch
      ## Last arg is type, the rest are the dimension lengths.

   PointerType* = ref object of AstNode
      rhs*: AstNode

   RecordType* = ref object of ASTBranch
      ## Fields are children
      base*: QualIdent # Optional base type.

   FieldList* = ref object of ASTBranch
      ## Field names are children.
      ty*: AstNode

   FPSection* = ref object of FieldList
      ## Same structure as FieldList, but the types are FormalType
      ## and can be marked as `in` parmeters with var.
      isVar*: bool

   FormalType* = ref object of TypeName
      ## Number of ARRAY OF prefixes for openarrays.
      nofArrays*: int

   FormalParameters* = ref object of ASTBranch
      ## Children are the FPSections.
      retTy*: AstNode
    
   ProcedureType* = ref object of AstNode
     ty*: FormalParameters

   StatementSequence* = ref object of ASTBranch

   IfStatement* = ref object of AstNode
      tests*: seq[AstNode]
         ## test expresion for each IF/ELSIF.  nil for last entry for ELSE.
      bodies*: seq[StatementSequence]
         ## bodies for tests at the same index in `tests`

   WhileStatement* = ref object of ASTBranch
      ## [0] - while condition
      ## [1] - while body
      ## [2] - condition of 1st ELSIF
      ## [3] - body for 1st ELSIF

   LabelRange* = ref object of ASTBranch
      ## 0 or 0..1 are populated.

   CaseLabelList* = ref object of ASTBranch
      ## 0..N LabelRanges

   Case* = ref object of ASTBranch
      ## 0 - CaseLabelList
      ## 1 - StatementList

   CaseStatement* = ref object of ASTBranch
      ## 0 - case expression
      ## 1..N - cases.

      
   SelectorKind* = enum
      skFieldAccess, 
      skArrayAccess,
      skPtrDeref,
      skTypeAssert  

   ExpressionList* = ref object of ASTBranch

   Selector* = ref object of AstNode
      kind*: SelectorKind
      arg*: AstNode
         ## Terminal for skFieldAccess, ExpressionList for 
         ## skArrayAccess, nil for skPtrDeref

   Designator* = ref object of ASTBranch
      ## Children are the selectors.
      leading*: QualIdent

   Assignment* = ref object of ASTBranch
      ## chld[0] == lhs designator
      ## chld[1] == rhs expression

   ProcedureCall* = ref object of ASTBranch
      ## chld[0] = function designator
      ## chld[1] = exprlist parameters.

   RepeatStatement* = ref object of ASTBranch
      ## 0 - body
      ## 1 - condition

   ForStatement* = ref object of ASTBranch
      ## 0 - loop var 
      ## 1 - start expression
      ## 2 - end expression
      ## 3 - by expression or nil if none supplied
      ## 4 - body statement sequence

   TypeDeclaration* = ref object of ASTBranch
      ## 0 - identdef
      ## 1 - type

   DeclarationSequence* = ref object of ASTBranch
      ## Sequence of const, type var and procedure declarations.

   ProcedureBody* = ref object of ASTBranch
      ## 0 - DeclarationSequence
      ## 1 - nil or StatementSequence
      ## 2 - nil, or return expression.

   ProcedureHeading* = ref object of ASTBranch
      ## 0 - IdentDef
      ## 1 - FormalParameters or nil if there are none.

   ProcedureDeclaration* = ref object of ASTBranch
      ## 0 - ProcedureHeading
      ## 1 - ProcedureBody
      ## 2 - Token for trailing indentifier

   Module* = ref object of ASTBranch
      ## 0 - terminal for module name
      ## 1 - ImportList or nil
      ## 2 - DeclarationSequence
      ## 3 - nil or StatementSequence
      ## 4 - ending identifier.

   Import* = ref object of ASTBranch
      ## 0 - Terminal module to import
      ## 1 - nil, or alias.  (reverse order from text, alias := module)

   ImportList* = ref object of ASTBranch
      ## 0-N Import.

const PrettyInd = 3

# Returns the position in the source this node starts at.
method startPos(n: AstNode) : int32  {.base.} = 
   assert false, "not implemented"   

# Returns the position in the source of the last character
# in this node.
method endPos(n: AstNode) : int32  {.base.} = 
   assert false, "not implemented"

## Pretty prints the AST to the given stream.
method toStr*(n: AstNode, str: Stream, src: string, ind: int = 0) {.base.} = 
   assert false, "not implemented"

proc indent(str: Stream, n: int) = 
   for i in 0..<n:
      str.write(" ")

proc toStrNodeChildren(ns: seq[ASTNode], str: Stream, src: string, ind: int) = 
   for p in ns:
      str.write("\N")
      if p == nil:
         indent(str, ind + PrettyInd)
         str.write("nil")
      else:
         toStr(p, str, src, PrettyInd + ind)

   str.write("]")

# Helper for nodes that have a single node as a "name"
proc toStrIdWrapper(n: AstNode, str: Stream, src: string, ind: int, name: string) = 
   indent(str, ind)
   str.write("[")
   str.write(name)
   str.write(" ")
   toStr(n, str, src, 0)
   str.write("]")

method toStr*(n: ASTBranch, str: Stream, src: string, ind: int = 0) = 
   indent(str, ind)
   str.write("[ASTBranch \N")
   toStrNodeChildren(n.chld, str, src, ind)
   

method startPos(t: Terminal) : int32 = 
   return t.tok.start

method endPos(t: Terminal) : int32 = 
   return t.tok.start + int32(t.tok.len) - 1

method toStr*(n: Terminal, str: Stream, src: string, ind: int = 0) = 
   indent(str, ind)
   str.write($n.tok.kind & "=" & text(n.tok, src))


method toStr*(n: QualIdent, str: Stream, src: string, ind: int = 0) = 
   indent(str, ind)
   str.write("[QualIdent")
   for c in n.pcs:
      str.write(" ", text(c, src))  
   str.write("]")


method toStr*(n: UnaryOp, str: Stream, src: string, ind: int = 0) = 
   indent(str, ind)
   str.write("[Unary ")
   str.write($n.op.kind) 
   str.write("\N")
   toStr(n.rhs, str, src, PrettyInd + ind)
   str.write("]")


method toStr*(n: Call, str: Stream, src: string, ind: int = 0) = 
   indent(str, ind)
   str.write("[Call ")
   toStr(n.id, str, src, 0)
   toStrNodeChildren(n.chld, str, src, ind)


method toStr*(n: SetLiteral, str: Stream, src: string, ind: int = 0) = 
   indent(str, ind)
   str.write("[SetLiteral")
   toStrNodeChildren(n.chld, str, src, ind)


method toStr*(n: BinaryOp, str: Stream, src: string, ind: int = 0) = 
   indent(str, ind)
   str.write("[BinaryOp " & $n.op.kind)
   str.write("\N")
   toStr(n.lhs, str, src, ind + PrettyInd)
   str.write("\N")
   toStr(n.rhs, str, src, ind + PrettyInd)
   str.write("]")


method toStr*(n: Var, str: Stream, src: string, ind: int = 0) = 
   toStrIdWrapper(n.id, str, src, ind, "Var")


method toStr*(n: ConstDeclaration, str: Stream, src: string, ind: int = 0) = 
   indent(str, ind)
   str.write("[ConstDeclaration\n")
   toStr(n.id, str, src, ind + PrettyInd)
   str.write("\N")
   toStr(n.rhs, str, src, ind + PrettyInd)
   str.write("]")


method toStr*(n: TypeName, str: Stream, src: string, ind: int = 0) = 
   toStrIdWrapper(n.id, str, src, ind, "TypeName")


method toStr*(n: ArrayType, str: Stream, src: string, ind: int = 0) = 
   indent(str, ind)
   str.write("[ArrayType")
   toStrNodeChildren(n.chld, str, src, ind)


method toStr*(n: PointerType, str: Stream, src: string, ind: int = 0) = 
   indent(str, ind)
   str.write("[PointerType\N")
   toStr(n.rhs, str, src, ind + PrettyInd)
   str.write("]")


method toStr*(n: ProcedureType, str: Stream, src: string, ind: int = 0) = 
   indent(str, ind)
   if n.ty != nil:
      str.write("[ProcedureType\N")
      toStr(n.ty, str, src, ind + PrettyInd)
      str.write("]")
   else:
      str.write("[ProcedureType]")


method toStr*(n: RecordType, str: Stream, src: string, ind: int = 0) = 
   indent(str, ind)
   str.write("[RecordType")
   if n.base != nil:
      str.write(" (")
      toStr(n.base, str, src, 0)
      str.write(")")
   toStrNodeChildren(n.chld, str, src, ind)


method toStr*(n: FieldList, str: Stream, src: string, ind: int = 0) = 
   indent(str, ind)
   str.write("[FieldList\N")
   toStr(n.ty, str, src, ind + PrettyInd)
   toStrNodeChildren(n.chld, str, src, ind)


method toStr*(n: FPSection, str: Stream, src: string, ind: int = 0) = 
   indent(str, ind)
   str.write("[FPSection\N")
   if n.isVar:
      indent(str, ind + PrettyInd)
      str.write("VAR\N")

   toStr(n.ty, str, src, ind + PrettyInd)
   toStrNodeChildren(n.chld, str, src, ind)


method toStr*(n: FormalType, str: Stream, src: string, ind: int = 0) = 
   indent(str, ind)
   str.write("[FormalType ")
   if n.nofArrays > 0:
      str.write("ARRRAY^" & $n.nofArrays & " ")
   toStr(n.id, str, src, 0)
   str.write("]")

method toStr*(n: FormalParameters, str: Stream, src: string, ind: int = 0) = 
   indent(str, ind)
   str.write("[FormalParameters\N")
   if n.retTy != nil:
      indent(str, ind + PrettyInd)
      str.write("ReturnType=")
      toStr(n.retTy, str, src)

   toStrNodeChildren(n.chld, str, src, ind)


method toStr*(n: StatementSequence, str: Stream, src: string, ind: int = 0) = 
   indent(str, ind)
   str.write("[StatementSequence")
   toStrNodeChildren(n.chld, str, src, ind)


method toStr*(n: IfStatement, str: Stream, src: string, ind: int = 0) = 
   indent(str, ind)
   str.write("[IfStatement\N")
   for i in 0..<len(n.tests):
      if n.tests[i] == nil:
         indent(str, ind + PrettyInd)
         str.write("ELSE\N")
      else:
         toStr(n.tests[i], str, src, ind + PrettyInd)
         str.write("\N")

      if n.tests[i] != nil:
         indent(str, ind + PrettyInd)
         str.write("THEN\N")
      toStr(n.bodies[i], str, src, ind + 2*PrettyInd)
      str.write("\N")

   str.write("]")


method toStr*(n: Designator, str: Stream, src: string, ind: int = 0) = 
   indent(str, ind)
   str.write("[Designator ")
   toStr(n.leading, str, src, 0)

   for c in n.chld:
      str.write("\N")
      toStr(c, str, src, ind + PrettyInd)

   str.write("]")

proc endsWithTypeAssert*(d: Designator) : bool = 
   let li = len(d.chld) - 1
   if li >= 0 and d.chld[li] of Selector:
      return Selector(d.chld[li]).kind == skTypeAssert
   else:
      return false


method toStr*(n: Selector, str: Stream, src: string, ind: int = 0) = 
   indent(str, ind)
   str.write("[Selector ")
   case n.kind
   of skFieldAccess:
      str.write("FIELD ")
      toStr(n.arg, str, src, 0)
   of skArrayAccess:
      str.write("[")
      toStr(n.arg, str, src, 0)
      str.write("]")
   of skPtrDeref:
      str.write("^")
   of skTypeAssert:
      str.write("TYPEASSERT ")
      toStr(n.arg, str, src, 0)

   str.write("]")


method toStr*(n: ExpressionList, str: Stream, src: string, ind: int = 0) = 
   indent(str, ind)
   str.write("[ExpressionList")
   toStrNodeChildren(n.chld, str, src, ind)

   
method toStr*(n: Assignment, str: Stream, src: string, ind: int = 0) = 
   indent(str, ind)
   str.write("[Assignment")
   toStrNodeChildren(n.chld, str, src, ind)


method toStr*(n: ProcedureCall, str: Stream, src: string, ind: int = 0) = 
   indent(str, ind)
   str.write("[ProcedureCall")
   toStrNodeChildren(n.chld, str, src, ind)


method toStr*(n: WhileStatement, str: Stream, src: string, ind: int = 0) = 
   indent(str, ind)
   str.write("[WhileStatement")
   toStrNodeChildren(n.chld, str, src, ind)


method toStr*(n: LabelRange, str: Stream, src: string, ind: int = 0) = 
   indent(str, ind)
   str.write("[LabelRange")
   toStrNodeChildren(n.chld, str, src, ind)


method toStr*(n: CaseLabelList, str: Stream, src: string, ind: int = 0) = 
   indent(str, ind)
   str.write("[CaseLabelList")
   toStrNodeChildren(n.chld, str, src, ind)


method toStr*(n: Case, str: Stream, src: string, ind: int = 0) = 
   indent(str, ind)
   str.write("[Case")
   toStrNodeChildren(n.chld, str, src, ind)


method toStr*(n: CaseStatement, str: Stream, src: string, ind: int = 0) = 
   indent(str, ind)
   str.write("[CaseStatement")
   toStrNodeChildren(n.chld, str, src, ind)


method toStr*(n: RepeatStatement, str: Stream, src: string, ind: int = 0) = 
   indent(str, ind)
   str.write("[RepeatStatement")
   toStrNodeChildren(n.chld, str, src, ind)

method toStr*(n: ForStatement, str: Stream, src: string, ind: int = 0) = 
   indent(str, ind)
   str.write("[ForStatement")
   toStrNodeChildren(n.chld, str, src, ind)


method toStr*(n: IdentDef, str: Stream, src: string, ind: int = 0) = 
   indent(str, ind)
   str.write($n.tok.kind & "=" & text(n.tok, src))
   if n.isExport:
      str.write("*")


method toStr*(n: TypeDeclaration, str: Stream, src: string, ind: int = 0) = 
   indent(str, ind)
   str.write("[TypeDeclaration")
   toStrNodeChildren(n.chld, str, src, ind)


method toStr*(n: DeclarationSequence, str: Stream, src: string, ind: int = 0) = 
   indent(str, ind)
   str.write("[DeclarationSequence")
   toStrNodeChildren(n.chld, str, src, ind)


method toStr*(n: ProcedureBody, str: Stream, src: string, ind: int = 0) = 
   indent(str, ind)
   str.write("[ProcedureBody")
   toStrNodeChildren(n.chld, str, src, ind)


method toStr*(n: ProcedureHeading, str: Stream, src: string, ind: int = 0) = 
   indent(str, ind)
   str.write("[ProcedureHeading")
   toStrNodeChildren(n.chld, str, src, ind)


method toStr*(n: ProcedureDeclaration, str: Stream, src: string, ind: int = 0) = 
   indent(str, ind)
   str.write("[ProcedureHeading")
   toStrNodeChildren(n.chld, str, src, ind)


method toStr*(n: Module, str: Stream, src: string, ind: int = 0) = 
   indent(str, ind)
   str.write("[Module")
   toStrNodeChildren(n.chld, str, src, ind)
