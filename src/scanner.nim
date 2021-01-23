## Simplest possible lexer.  Just keeping all text in memory.
## Working off 2016 Oberon07 report.  Ignoring UTF8 now for simplicity.
type
  TokenKind* {.size: 2.} = enum
    EOF, 
    GARBAGE, # Unknown char/token.
    Id, ConstInt, ConstHex, ConstHexString, ConstReal, ConstString

    # Punctuation and operators.
    PLUS, MINUS, ASTERISK, FSLASH, TILDE,
    AMPERSAND, DOT, COMMA, SEMI, BAR, LPAREN, LBRACKET, LBRACE, 
    COL_EQ, CARET, EQ, OCTOTHORPE, LT, GT, LTE, GTE, DOTDOT, COLON, 
    RPAREN, RBRACKET, RBRACE, 

    # Keywords.
    KARRAY, KBEGIN, KEND, KBY, KCASE, KCONST, KDIV, KDO, KELSE, KELSIF, 
    KFALSE, KFOR, KIF, KIMPORT, KIN, KIS, KMOD, KMODULE, KNIL, KOF, KOR, 
    KPOINTER, KPROCEDURE, KRECORD, KREPEAT, KRETURN, KTHEN, KTO, KTRUE, 
    KTYPE, KUNTIL, KVAR, KWHILE

    # Prim types
    CHAR, REAL, BOOLEAN, INTEGER, BYTE, SET,

    # Buitin functions.
    ABS, FLT, LSL, ASR, CHR, INC, NEW, ROR, ASSERT, DEC, INCL, ODD, 
    EXCL, ORD, UNPK, FLOOR, LEN, PACK

  Token* = object 
     kind*: TokenKind
     # Index into source string.
     start*: int32
     len*: int16

  Scanner* = ref object
     txt: string
     cur*: Token
     prev*: Token
     pos*: int32

let WS = [' ', '\t', '\r', '\n']

#TODO the code for lexing reals should probably be broken out so we
#     can also use it externally to break the real into component parts
#     for turning into a constant.

## Reads the next token from the string, and returns the kind.
## Returns EOF if there are no tokens left.
proc next*(s: Scanner) : TokenKind = 
  template tok(tk: TokenKind, tlen: untyped = 1) = 
    s.cur = Token(kind: tk, start: s.pos, len: int16(tlen))

  proc isLetter(c: char) : bool = 
    return (c >= 'A' and c <= 'Z') or (c >= 'a' and c <= 'z')

  proc isDigit(c: char) : bool = 
    return (c >= '0' and c <= '9')

  proc isHexDigit(c: char) : bool = 
    return c >= 'A' and c <= 'F'

  proc isIdentTrailingChar(c: char) : bool = 
     return isLetter(c) or isDigit(c)

  proc isWordSep(c: char) : bool = 
    return c in WS or c == '\0' or not isIdentTrailingChar(c)

  proc la(k: int = 1) : char = 
     let np = s.pos + k
     if np >= len(s.txt):
       return '\0'
     else:
       return s.txt[np]

  proc kcheck(k: string, kind: TokenKind) : bool = 
     if (len(s.txt) - s.pos) >= len(k):
       if equalMem(s.txt[s.pos].addr, k[0].unsafeAddr, len(k)) and isWordSep(la(len(k))):
         tok(kind, len(k))
         return true
       else:
         return false
     else:
       return false
  
  proc acceptKW() : bool = 
    case s.txt[s.pos]
    of 'A':
      return kcheck("ARRAY", KARRAY) or kcheck("ABS", ABS)
    of 'B':
      return kcheck("BEGIN", KBEGIN) or kcheck("BY", KBY) or 
             kcheck("BOOLEAN", BOOLEAN) or kcheck("BYTE", BYTE)
    of 'C':
      return kcheck("CASE", KCASE) or kcheck("CONST", KCONST) or 
             kcheck("CHAR", CHAR) or kcheck("CHR", CHR)
    of 'D':
      return kcheck("DIV", KDIV) or kcheck("DO", KDO)
    of 'E':
      return kcheck("ELSIF", KELSIF) or kcheck("ELSE", KELSE) or
             kcheck("EXCL", EXCL) or kcheck("END", KEND)
    of 'F':
      return kcheck("FALSE", KFALSE) or kcheck("FOR", KFOR) or kcheck("FLT", FLT) or
             kcheck("FLOOR", FLOOR)
    of 'I':
      return kcheck("IF", KIF) or kcheck("IN", KIN) or kcheck("IS", KIS) or
             kcheck("INTEGER", INTEGER) or kcheck("INCL", INCL)
    of 'M':
      return kcheck("MODULE", KMODULE) or kcheck("MOD", KMOD)
    of 'N':
      return kcheck("NIL", KNIL) or kcheck("NEW", NEW)
    of 'O':
      return kcheck("OF", KOF) or kcheck("OR", KOR) or kcheck("ODD", ODD) or
             kcheck("ORD", ORD)
    of 'P':
      return kcheck("POINTER", KPOINTER) or kcheck("PROCEDURE", KPROCEDURE) or
             kcheck("PACK", PACK)
    of 'R':
      return kcheck("RECORD", KRECORD) or kcheck("REPEAT", KREPEAT) or
             kcheck("RETURN", KRETURN) or kcheck("REAL", REAL) or 
             kcheck("ROR", ROR)
    of 'S':
      return kcheck("SET", SET)
    of 'T':
      return kcheck("THEN", KTHEN) or kcheck("TO", KTO) or kcheck("TRUE", KTRUE) or
             kcheck("TYPE", KTYPE)
    of 'U':
      return kcheck("UNTIL", KUNTIL) or kcheck("UNPK", UNPK)
    of 'V':
      return kcheck("VAR", KVAR)
    of 'W':
      return kcheck("WHILE", KWHILE)

    else:
      return false

  # Starts at char after E for real.
  proc lexScaleFactor(ix: int) = 
    if ix >= len(s.txt):
      tok(GARBAGE, ix - s.pos)
    else:
      var i = ix
      var c = s.txt[i]

      if c == '+' or c == '-':
        inc(i)
        c = s.txt[i]
        if i >= len(s.txt):
          tok(GARBAGE, i - s.pos)
          return

      while i < len(s.txt):
        c = s.txt[i]

        if isDigit(c):
          inc(i)
        elif c == '.':
          tok(GARBAGE, i + 1 - s.pos)
          return
        elif isWordSep(c):
          break
        else:
          tok(GARBAGE, i + 1 - s.pos)
          return

      tok(ConstReal, i - s.pos)

  # Starts at character after dot.
  proc lexReal(ix: int) = 
    if ix >= len(s.txt) or not isDigit(s.txt[ix]):
      tok(GARBAGE, ix - s.pos)
    else:
      var i = ix + 1

      while i < len(s.txt):
        let c = s.txt[i]

        if c == 'E':
          lexScaleFactor(i+1)
          return
        elif c == '.':
          tok(GARBAGE, i + 1 - s.pos)
          return 
        elif isWordSep(c):
          break
        elif isDigit(c):
          inc(i)
        else:
          tok(GARBAGE, i + 1 - s.pos)
          return

      tok(ConstReal, i - s.pos)

  proc lexNumberOrHexString() = 
    var i = s.pos
    var hexSeen = false

    while i < len(s.txt):
      let c = s.txt[i]

      if c == '.':
        lexReal(i+1)
        return
      elif c == 'H':
        tok(ConstHex, i + 1 - s.pos)
        return
      elif c == 'X':
        tok(ConstHexString, i + 1 - s.pos)
        return
      elif isHexDigit(c):
        inc(i)
        hexSeen = true
      elif isDigit(c):
        inc(i)
      elif isWordSep(c):
        break
      else:
        tok(GARBAGE, i - s.pos)
        return 

    if hexSeen:
      # no trailing H
      tok(GARBAGE, i - s.pos)
    else:
      tok(ConstInt, i - s.pos)
    
  s.prev = s.cur
  while s.pos < len(s.txt) and s.txt[s.pos] in WS:
    inc(s.pos)

  if s.pos >= len(s.txt):
    s.cur.kind = EOF
    s.cur.len = 0
  else:
    case s.txt[s.pos]
    of '+': tok(PLUS)
    of '-': tok(MINUS)
    of '*': tok(ASTERISK)
    of '/': tok(FSLASH)
    of '~': tok(TILDE)
    of '&': tok(AMPERSAND)
    of '.': 
       if la() == '.':
         tok(DOTDOT, 2)
       else:
         tok(DOT)
    of ',': tok(COMMA)
    of ';': tok(SEMI)
    of '|': tok(BAR)
    of '(': tok(LPAREN)
    of '[': tok(LBRACKET)
    of '{': tok(LBRACE)
    of ':':
       if la() == '=':
         tok(COL_EQ, 2)
       else:
         tok(COLON)

    of '^': tok(CARET)
    of '=': tok(EQ)
    of '#': tok(OCTOTHORPE)
    of '<': 
       if la() == '=':
         tok(LTE, 2)
       else:
         tok(LT)
    of '>':
       if la() == '=':
         tok(GTE, 2)
       else:
         tok(GT)
    of ')': tok(RPAREN)
    of ']': tok(RBRACKET)
    of '}': tok(RBRACE)
    of '"': 
      var i = s.pos + 1
      s.cur.kind = EOF
      while i < len(s.txt):
        let c = s.txt[i]

        if c in ['\0', '\n', '\r']:
          tok(GARBAGE, i - s.pos)
        elif c == '"':
          tok(ConstString, i + 1 - s.pos)
          break

        inc(i)

      if s.cur.kind == EOF:
        tok(GARBAGE, i - s.pos)

    else:
      if not acceptKw():
        let c = s.txt[s.pos]
        if isLetter(c):
          var i = s.pos + 1
          while i < len(s.txt) and isIdentTrailingChar(s.txt[i]):
            inc(i)
          tok(Id, i - s.pos)
        elif isDigit(c):
          lexNumberOrHexString()
        else:
          tok(GARBAGE)

  s.pos += s.cur.len
  return s.cur.kind


proc initScanner*(txt: string) : Scanner = 
  result = Scanner(txt: txt, pos: 0)
  discard next(result)

## Extracts a string from the original source from a token.
proc text*(t: Token, src: string) : string = 
   return src[t.start..t.start + t.len - 1]

when isMainModule:
  import strformat 

  proc tok(s: string) : TokenKind = 
    let sc = initScanner(s)
    let rv = sc.cur.kind
    echo &"TOK={rv}"
    return rv

  assert tok("1") == ConstInt
  assert tok("1.") == GARBAGE
  assert tok("1.0") == ConstReal
  assert tok("1.0E") == GARBAGE
  assert tok("1.0E1") == ConstReal
  assert tok("1.0E+1") == ConstReal
  assert tok("1.0E-1") == ConstReal
  assert tok("1EF") == GARBAGE
  assert tok("1EFH") == ConstHex
  assert tok("4.567E8") == ConstReal

  assert tok("\"har") == GARBAGE
  assert tok("\"har\n") == GARBAGE
  assert tok("\"har\"") == ConstString
  assert tok("0FEX") == ConstHexString

