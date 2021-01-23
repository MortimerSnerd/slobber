import 
   scanner, streams

type
   AstNode* = ref object of RootRef
      discard

   ConstDeclaration* = ref object of AstNode
      id*: IdentDef
      rhs*: AstNode

   ParentOfMany* = ref object of AstNode
      chld*: seq[ASTNode]

   Terminal* = ref object of AstNode
      ## Id or a literal.
      tok*: Token

   IdentDef* = ref object of AstNode
      id*: Token
      public*: bool

   QualIdent* = ref object of AstNode
      pcs*: seq[Token]

   UnaryOp* = ref object of AstNode
     op*: Token
     rhs*: AstNode

   BinaryOp* = ref object of AstNode
     op*: Token
     lhs*: AstNode
     rhs*: AstNode

   ActualParameters* = ref object of ParentOfMany
   SetLiteral* = ref object of ParentOfMany
      ldelim*: Token
      rdelim*: Token

   Call* = ref object of ParentOfMany
      id*: QualIdent

   Var* = ref object of AstNode
      id*: QualIdent

   TypeName* = ref object of AstNode
      id*: QualIdent

   ArrayType* = ref object of ParentOfMany
      ## Last arg is type, the rest are the dimension lengths.

   PointerType* = ref object of AstNode
      rhs*: AstNode

   RecordType* = ref object of ParentOfMany
      ## Fields are children
      base*: QualIdent # Optional base type.

   FieldList* = ref object of ParentOfMany
      ## Field names are children.
      ty*: AstNode

   FPSection* = ref object of FieldList
      ## Same structure as FieldList, but the types are FormalType
      ## and can be marked as `in` parmeters with var.
      isVar*: bool

   FormalType* = ref object of TypeName
      ## Number of ARRAY OF prefixes for openarrays.
      nofArrays*: int

   FormalParameters* = ref object of ParentOfMany
      ## Children are the FPSections.
      retTy*: AstNode
    
   ProcedureType* = ref object of AstNode
     ty*: FormalParameters

   StatementSequence* = ref object of ParentOfMany
      

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

method toStr*(n: ParentOfMany, str: Stream, src: string, ind: int = 0) = 
   indent(str, ind)
   str.write("[ParentOfMany \N")
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

method toStr*(n: IdentDef, str: Stream, src: string, ind: int = 0) = 
   indent(str, ind)
   str.write("[IdentDef ")
   if n.public:
      str.write("*")
   str.write(text(n.id, src))
   str.write("]")


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
   str.write("[StatementSequence\N")
   toStrNodeChildren(n.chld, str, src, ind)

