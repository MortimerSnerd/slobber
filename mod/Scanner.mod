MODULE Scanner;
IMPORT Files, Strings;

CONST 
   (* For now, have a ridiculous buffer size to avoid 
      dealing with the Texts module, or buffering. *)
   BufSz = 1024 * 128;

    (* Tokens *)
    EOF* = 0; 
    GARBAGE* = 66; (* Unknown char/token. *)
    Id* = 1; ConstInt* = 2; ConstHex* = 3; ConstHexString* = 4; ConstReal* = 5; 
    ConstString* = 6;

    (* Punctuation and operators. *)
    PLUS* = 7; MINUS* = 8; ASTERISK* = 9; FSLASH* = 10; TILDE* = 11;
    AMPERSAND* = 12; DOT* = 13; COMMA* = 14; SEMI* = 15; BAR* = 16; 
    LPAREN* = 17; LBRACKET* = 18; LBRACE* = 19; COLEQ* = 20; CARET* = 21; 
    EQ* = 22; OCTOTHORPE* = 23; LT* = 24; GT* = 25; LTE* = 26; GTE* = 27; 
    DOTDOT* = 28; COLON* = 29; RPAREN* = 30; RBRACKET* = 31; RBRACE* = 32;

    (* Keywords *)
    KARRAY* = 33; KBEGIN* = 34; KEND* = 35; KBY* = 36; KCASE* = 37; 
    KCONST* = 38; KDIV* = 39; KDO* = 40; KELSE* = 41; KELSIF* = 42; 
    KFALSE* = 43; KFOR* = 44; KIF* = 45; KIMPORT* = 46; KIN* = 47; 
    KIS* = 48; KMOD* = 49; KMODULE* = 50; KNIL* = 51; KOF* = 52; KOR* = 53; 
    KPOINTER* = 54; KPROCEDURE* = 55; KRECORD* = 56; KREPEAT* = 57; 
    KRETURN* = 58; KTHEN* = 59; KTO* = 60; KTRUE* = 61; KTYPE* = 62; 
    KUNTIL* = 63; KVAR* = 64; KWHILE* = 65;
    NumTok=67;

    CR=0DX; LF=0AX; TAB=9X; DQUOT=22X;

TYPE
   TokKind* = INTEGER;

   Token* = RECORD
               kind*: TokKind;
               start*, len*: INTEGER
            END;


   T* = POINTER TO TDesc;
   TDesc* = RECORD
           buf*: ARRAY BufSz OF CHAR;
           pos*, len*, line*: INTEGER;
           cur*, prev*: Token
        END;

   (* Records the state of the lexer, so it can be resumed
      at an earlier point for backtracking, via Mark() and Rewind() *)
   LexState* = RECORD
      pos: INTEGER;
      cur, prev: Token
   END;

VAR
   (* Lookup table from token number to a string *)
   TokenNames*: ARRAY NumTok OF ARRAY 32 OF CHAR;

PROCEDURE ColForPos*(p:T; pos: INTEGER): INTEGER;
VAR rv, i: INTEGER;
    found: BOOLEAN;
BEGIN
   rv := 0;
   i := pos;
   found := FALSE;
   WHILE ~found & (i > 0) DO
      IF (p.buf[i] = CR) OR (p.buf[i] = LF) THEN
         found := TRUE
      ELSE
         INC(rv); DEC(i)
      END
   END;

   RETURN rv
END ColForPos; 

PROCEDURE LineForPos*(p: T; pos: INTEGER): INTEGER;
VAR rv, i: INTEGER;
BEGIN
   rv := 1;
   i := pos;
   WHILE i >= 0 DO
      (* being cheap and only checking for LF.  Too bad for old
         CR only line separators *)
      IF p.buf[i] = LF THEN
         INC(rv)
      END;
      DEC(i)
   END;
   RETURN rv
END LineForPos;
         


(* Calculates the column number from the current position *)
PROCEDURE CurCol*(p:T): INTEGER;
BEGIN
   RETURN ColForPos(p, p.pos)
END CurCol; 

(* Saves current lexing state into `state` *)
PROCEDURE Mark*(p:T; VAR state: LexState);
BEGIN
   state.pos := p.pos;
   state.cur := p.cur;
   state.prev := p.prev
END Mark; 

(* Rewinds to a previous lexer position saved by Mark() *)
PROCEDURE Rewind*(VAR p:T; state: LexState);
BEGIN
   p.pos := state.pos;
   p.cur := state.cur;
   p.prev := state.prev
END Rewind;

PROCEDURE NewFromString*(txt: ARRAY OF CHAR): T;
VAR scan: T;
BEGIN
   NEW(scan);
   scan.pos := 0;
   scan.buf[0] := 0X;
   scan.len := Strings.Length(txt);
   scan.line := 1;
   Strings.Append(txt, scan.buf);
   RETURN scan
END NewFromString;

PROCEDURE NewFromFile*(fname: ARRAY OF CHAR): T;
VAR fh: Files.File;
    ride: Files.Rider;
    scan: T;
BEGIN
   NEW(scan);
   scan.pos := 0;
   scan.line := 1;
   fh := Files.Old(fname);
   scan.len := Files.Length(fh);
   ASSERT(scan.len < BufSz);
   Files.Set(ride, fh, 0);
   Files.ReadString(ride, scan.buf);
   Files.Close(fh);
   RETURN scan 
END NewFromFile;

PROCEDURE IsWs(c: CHAR): BOOLEAN;
BEGIN
   RETURN (c = " ") OR (c = TAB) OR (c = CR) OR (c = LF)
END IsWs;

PROCEDURE la(scan: T; n: INTEGER) : CHAR; 
VAR np: INTEGER;
    rv: CHAR;
BEGIN
   np := scan.pos + n;
   IF np >= scan.len THEN
      rv := 0X
   ELSE
      rv := scan.buf[np]
   END;
   RETURN rv
END la;

PROCEDURE SkipComment(VAR scan: T);
VAR nesting: INTEGER;
    c: CHAR;
BEGIN
   IF (la(scan, 0) = "(") & (la(scan, 1) = "*") THEN
      nesting := 1;
      scan.pos := scan.pos + 2;
      WHILE (scan.pos < scan.len) & (nesting > 0) DO
         c := scan.buf[scan.pos];
         IF (c = "*") & (la(scan, 1) = ")") THEN
            scan.pos := scan.pos + 2;
            DEC(nesting)
         ELSIF (c = "(") & (la(scan, 1) = "*") THEN
            scan.pos := scan.pos + 2;
            INC(nesting)
         ELSE
            IF c = LF THEN INC(scan.line) END;
            INC(scan.pos)
         END
      END
   END
END SkipComment;

PROCEDURE SkipWs(VAR scan:T);
BEGIN
   WHILE (scan.pos < scan.len) & IsWs(scan.buf[scan.pos]) DO
      IF scan.buf[scan.pos] = LF THEN INC(scan.line) END;
      INC(scan.pos)
   ELSIF (scan.pos < scan.len) & (la(scan, 0) = "(") & (la(scan, 1) = "*") DO
      SkipComment(scan)
   END
END SkipWs;

PROCEDURE tok(VAR scan: T; k: TokKind; l: INTEGER);
BEGIN
   scan.cur.kind := k;
   scan.cur.start := scan.pos;
   scan.cur.len := l 
END tok;

PROCEDURE isDigit(c: CHAR): BOOLEAN;
BEGIN
   RETURN (c >= "0") & (c <= "9")
END isDigit;

PROCEDURE IsAlpha(c: CHAR): BOOLEAN;
BEGIN
   RETURN ((c >= "A") & (c <= "Z")) OR ((c >= "a") & (c <= "z"))
END IsAlpha;

PROCEDURE isIdentTrailingChar(c: CHAR): BOOLEAN;
BEGIN
     RETURN IsAlpha(c) OR isDigit(c)
END isIdentTrailingChar;

PROCEDURE isWordSep(c: CHAR): BOOLEAN;
BEGIN
   RETURN IsWs(c) OR (c = 0X) OR ~isIdentTrailingChar(c)
END isWordSep;

(* Starts at char after E for real. *)
PROCEDURE lexScaleFactor(VAR scan: T; i: INTEGER);
VAR c: CHAR;
    stop: BOOLEAN;
BEGIN
   scan.cur.kind := EOF;
   IF i >= scan.len THEN
      tok(scan, GARBAGE, i - scan.pos)
   ELSE
      c := scan.buf[i];
      IF (c = "+") OR (c = "-") THEN INC(i) END;
      c := scan.buf[i];
      IF i >= scan.len THEN
         tok(scan, GARBAGE, i - scan.pos)
      ELSE
         stop := FALSE;
         WHILE ~stop & (i < scan.len) DO
            c := scan.buf[i];
            IF isDigit(c) THEN
               INC(i)
            ELSIF c = "." THEN
               tok(scan, GARBAGE, i + 1 - scan.pos); stop := TRUE
            ELSIF isWordSep(c) THEN
               stop := TRUE
            ELSE
               tok(scan, GARBAGE, i + 1 - scan.pos); stop := TRUE
            END
         END
      END
   END;

   IF scan.cur.kind = EOF THEN
      tok(scan, ConstReal, i - scan.pos)
   END
END lexScaleFactor;


(* Starts at character after dot. *)
PROCEDURE lexReal(VAR scan: T; i: INTEGER);
VAR c: CHAR;
    stop: BOOLEAN;
BEGIN
   IF (i >= scan.len) OR ~isDigit(scan.buf[i]) THEN
      tok(scan, GARBAGE, i - scan.pos)
   ELSE
      INC(i);
      stop := FALSE;
      scan.cur.kind := EOF;
      WHILE ~stop & (i < scan.len) DO
         c := scan.buf[i];
         IF c = "E" THEN
            lexScaleFactor(scan, i+1); stop := TRUE
         ELSIF c = "." THEN
            tok(scan, GARBAGE, i + 1 - scan.pos); stop := TRUE
         ELSIF isWordSep(c) THEN
            stop := TRUE
         ELSIF isDigit(c) THEN
            INC(i)
         ELSE
            tok(scan, GARBAGE, i + 1 - scan.pos); stop := TRUE
         END
      END
   END;
   IF scan.cur.kind = EOF THEN tok(scan, ConstReal, i - scan.pos) END
END lexReal;

PROCEDURE isHexDigit(c: CHAR): BOOLEAN;
BEGIN
   RETURN (c >= "A") & (c <= "F")
END isHexDigit;

PROCEDURE lexNumberOrHexString(VAR scan: T);
VAR i: INTEGER;
    hexSeen, stop: BOOLEAN;
    c: CHAR;
BEGIN
   i := scan.pos;
   hexSeen := FALSE;
   stop := FALSE;
   scan.cur.kind := EOF;
   WHILE ~stop & (i < scan.len) DO
      c := scan.buf[i];
      IF c = "." THEN
         (* Check for .. operator right against a number *)
         IF la(scan, 1) = "." THEN
            tok(scan, ConstInt, i - scan.pos)
         ELSE
            lexReal(scan, i+1)
         END;
         stop := TRUE
      ELSIF c = "H" THEN
         tok(scan, ConstHex, i + 1 - scan.pos);
         stop := TRUE
      ELSIF c = "X" THEN
         tok(scan, ConstHexString, i + 1 - scan.pos);
         stop := TRUE
      ELSIF  isHexDigit(c) THEN
         INC(i);
         hexSeen := TRUE
      ELSIF isDigit(c) THEN
         INC(i)
      ELSIF isWordSep(c) THEN
         stop := TRUE
      ELSE
         tok(scan, GARBAGE, i - scan.pos);
         stop := TRUE
      END
   END;
   IF scan.cur.kind = EOF THEN
      IF hexSeen THEN
        (* no trailing H *)
        tok(scan, GARBAGE, i - scan.pos)
      ELSE
        tok(scan, ConstInt, i - scan.pos)
      END
   END
END lexNumberOrHexString;

PROCEDURE lexString(VAR scan: T);
VAR i: INTEGER; cancelled: BOOLEAN; c: CHAR;
BEGIN
   i := scan.pos + 1;
   cancelled := FALSE;
   scan.cur.kind := EOF;
   WHILE ~cancelled & (i < scan.len) DO
     c := scan.buf[i];
     IF (c = 0X) OR (c = LF) OR (c = CR) THEN
       tok(scan, GARBAGE, i - scan.pos)
     ELSIF c = DQUOT THEN
       tok(scan, ConstString, i + 1 - scan.pos);
       cancelled := TRUE
     END;
     INC(i)
  END;
  IF scan.cur.kind = EOF THEN tok(scan, GARBAGE, i - scan.pos) END
END lexString;

PROCEDURE TokEql(s: T; pos: INTEGER; k: ARRAY OF CHAR): BOOLEAN;
VAR i, klen: INTEGER;
    rv: BOOLEAN;
BEGIN
   klen := LEN(k) - 1;  (* -1 to ignore null *)
   IF (pos + klen) > s.len THEN
      rv := FALSE
   ELSE
      i := 0;
      rv := TRUE;
      WHILE rv & (i < klen) DO
         IF s.buf[i+pos] # k[i] THEN
            rv := FALSE
         ELSE
            INC(i)
         END
      END
   END;
   RETURN rv
END TokEql;

PROCEDURE Eql*(s: T; a, b: Token): BOOLEAN;
VAR rv: BOOLEAN;
    i: INTEGER;
BEGIN
   IF a.len = b.len THEN
      IF a.start = b.start THEN
         rv := TRUE
      ELSE
         i := 0;
         rv := TRUE;
         WHILE rv & (i < a.len) DO
            IF s.buf[i+a.start] # s.buf[i+b.start] THEN
               rv := FALSE
            ELSE
               INC(i)
            END
         END
      END
   ELSE
      rv := FALSE
   END;
   RETURN rv
END Eql;

PROCEDURE kcheck(VAR s: T; k: ARRAY OF CHAR; kind: TokKind): BOOLEAN;
VAR rv: BOOLEAN;
BEGIN
   (* LEN(k)-1 - keep forgetting len includes the \0 *)
   IF TokEql(s, s.pos, k) & isWordSep(la(s, LEN(k)-1)) THEN   
      tok(s, kind, LEN(k)-1);
      rv := TRUE
   ELSE
      rv := FALSE
   END;
   RETURN rv
END kcheck;

PROCEDURE AcceptKw(VAR scan: T): BOOLEAN;
VAR rv: BOOLEAN;
BEGIN    
   rv := FALSE;
   IF scan.buf[scan.pos] =  "A" THEN
      rv :=  kcheck(scan, "ARRAY", KARRAY)
   ELSIF scan.buf[scan.pos] = "B" THEN
      rv :=  kcheck(scan, "BEGIN", KBEGIN) OR kcheck(scan, "BY", KBY)
   ELSIF scan.buf[scan.pos] =  "C" THEN
      rv :=  kcheck(scan, "CASE", KCASE) OR kcheck(scan, "CONST", KCONST)
   ELSIF scan.buf[scan.pos] =  "D" THEN
      rv :=  kcheck(scan, "DIV", KDIV) OR kcheck(scan, "DO", KDO)
   ELSIF scan.buf[scan.pos] =  "E" THEN
      rv :=  kcheck(scan, "ELSIF", KELSIF) OR kcheck(scan, "ELSE", KELSE) OR
             kcheck(scan, "END", KEND)
   ELSIF scan.buf[scan.pos] =  "F" THEN
      rv :=  kcheck(scan, "FALSE", KFALSE) OR kcheck(scan, "FOR", KFOR) 
   ELSIF scan.buf[scan.pos] =  "I" THEN
      rv :=  kcheck(scan, "IF", KIF) OR kcheck(scan, "IN", KIN) 
             OR kcheck(scan, "IS", KIS) OR kcheck(scan, "IMPORT", KIMPORT)
   ELSIF scan.buf[scan.pos] =  "M" THEN
      rv :=  kcheck(scan, "MODULE", KMODULE) OR kcheck(scan, "MOD", KMOD)
   ELSIF scan.buf[scan.pos] =  "N" THEN
      rv :=  kcheck(scan, "NIL", KNIL)
   ELSIF scan.buf[scan.pos] =  "O" THEN
      rv :=  kcheck(scan, "OF", KOF) OR kcheck(scan, "OR", KOR) 
   ELSIF scan.buf[scan.pos] =  "P" THEN
      rv :=  kcheck(scan, "POINTER", KPOINTER) 
             OR kcheck(scan, "PROCEDURE", KPROCEDURE) 
   ELSIF scan.buf[scan.pos] =  "R" THEN
      rv :=  kcheck(scan, "RECORD", KRECORD) 
             OR kcheck(scan, "REPEAT", KREPEAT) 
             OR kcheck(scan, "RETURN", KRETURN)
   ELSIF scan.buf[scan.pos] =  "T" THEN
      rv :=  kcheck(scan, "THEN", KTHEN) OR kcheck(scan, "TO", KTO) 
             OR kcheck(scan, "TRUE", KTRUE) 
             OR kcheck(scan, "TYPE", KTYPE)
   ELSIF scan.buf[scan.pos] =  "U" THEN
      rv :=  kcheck(scan, "UNTIL", KUNTIL)
   ELSIF scan.buf[scan.pos] =  "V" THEN
      rv :=  kcheck(scan, "VAR", KVAR)
   ELSIF scan.buf[scan.pos] =  "W" THEN
      rv :=  kcheck(scan, "WHILE", KWHILE)
   END;
   RETURN rv
END AcceptKw;

PROCEDURE LexIdentifier(VAR scan: T);
VAR i: INTEGER;
    stop: BOOLEAN;
BEGIN
   i := scan.pos;
   IF (i < scan.len) & IsAlpha(scan.buf[i]) THEN
      stop := FALSE;
      INC(i);
      WHILE ~stop & (i < scan.len) & isIdentTrailingChar(scan.buf[i]) DO
         INC(i)
      END;
      tok(scan, Id, i - scan.pos)
   ELSE
      tok(scan, GARBAGE, 1)
   END
END LexIdentifier;
   
PROCEDURE Next*(VAR scan: T) : TokKind;
VAR c: CHAR;
BEGIN
   SkipWs(scan);
   scan.prev := scan.cur;
   scan.cur.kind := EOF;
   IF scan.pos >= scan.len THEN
      scan.cur.len := 0
   ELSE
      c := scan.buf[scan.pos];
      IF c = "+" THEN tok(scan, PLUS, 1)
      ELSIF c = "-" THEN tok(scan, MINUS, 1)
      ELSIF c = "*" THEN tok(scan, ASTERISK, 1)
      ELSIF c = "/" THEN tok(scan, FSLASH, 1)
      ELSIF c = "~" THEN tok(scan, TILDE, 1)
      ELSIF c = "&" THEN tok(scan, AMPERSAND, 1)
      ELSIF c = "." THEN IF la(scan, 1) = "." THEN tok(scan, DOTDOT, 2) 
                                              ELSE tok(scan, DOT, 1) END
      ELSIF c = "," THEN tok(scan, COMMA, 1)
      ELSIF c = ";" THEN tok(scan, SEMI, 1)
      ELSIF c = "|" THEN tok(scan, BAR, 1)
      ELSIF c = "(" THEN tok(scan, LPAREN, 1)
      ELSIF c = "[" THEN tok(scan, LBRACKET, 1)
      ELSIF c = "{" THEN tok(scan, LBRACE, 1)
      ELSIF c = ":" THEN IF la(scan, 1) = "=" THEN tok(scan, COLEQ, 2) 
                                              ELSE tok(scan, COLON, 1) END

      ELSIF c = "^" THEN tok(scan, CARET, 1)
      ELSIF c = "=" THEN tok(scan, EQ, 1)
      ELSIF c = "#" THEN tok(scan, OCTOTHORPE, 1)
      ELSIF c = "<" THEN IF la(scan, 1) = "=" THEN tok(scan, LTE, 2) 
                                              ELSE tok(scan, LT, 1) END
      ELSIF c = ">" THEN IF la(scan, 1) = "=" THEN tok(scan, GTE, 2) 
                                              ELSE tok(scan, GT, 1) END
      ELSIF c = ")" THEN tok(scan, RPAREN, 1)
      ELSIF c = "]" THEN tok(scan, RBRACKET, 1)
      ELSIF c = "}" THEN tok(scan, RBRACE, 1)         
      ELSIF c = DQUOT THEN lexString(scan)
      ELSIF IsAlpha(c) THEN 
         IF ~AcceptKw(scan) THEN
            LexIdentifier(scan)
         END
      ELSIF isDigit(c) THEN
         lexNumberOrHexString(scan)
      END;

      scan.pos := scan.pos + scan.cur.len
   END;
   RETURN scan.cur.kind
END Next;

BEGIN
   TokenNames[EOF] := "EOF"; 
   TokenNames[GARBAGE] := "GARBAGE6"; 
   TokenNames[Id] := "Id"; 
   TokenNames[ConstInt] := "ConstInt";
   TokenNames[ConstHex] := "ConstHex";
   TokenNames[ConstHexString] := "ConstHexString";
   TokenNames[ConstReal] := "ConstReal";
   TokenNames[ConstString] := "ConstString";
   TokenNames[PLUS] := "PLUS";
   TokenNames[MINUS] := "MINUS";
   TokenNames[ASTERISK] := "ASTERISK";
   TokenNames[FSLASH] := "FSLASH";
   TokenNames[TILDE] := "TILDE";
   TokenNames[AMPERSAND] := "AMPERSAND";
   TokenNames[DOT] := "DOT";
   TokenNames[COMMA] := "COMMA";
   TokenNames[SEMI] := "SEMI";
   TokenNames[BAR] := "BAR";
   TokenNames[LPAREN] := "LPAREN";
   TokenNames[LBRACKET] := "LBRACKET";
   TokenNames[LBRACE] := "LBRACE";
   TokenNames[COLEQ] := "COLEQ";
   TokenNames[CARET] := "CARET";
   TokenNames[EQ] := "EQ";
   TokenNames[OCTOTHORPE] := "OCTOTHORPE";
   TokenNames[LT] := "LT";
   TokenNames[GT] := "GT";
   TokenNames[LTE] := "LTE";
   TokenNames[GTE] := "GTE";
   TokenNames[DOTDOT] := "DOTDOT";
   TokenNames[COLON] := "COLON";
   TokenNames[RPAREN] := "RPAREN";
   TokenNames[RBRACKET] := "RBRACKET";
   TokenNames[RBRACE] := "RBRACE";
   TokenNames[KARRAY] := "KARRAY";
   TokenNames[KBEGIN] := "KBEGIN";
   TokenNames[KEND] := "KEND";
   TokenNames[KBY] := "KBY";
   TokenNames[KCASE] := "KCASE";
   TokenNames[KCONST] := "KCONST";
   TokenNames[KDIV] := "KDIV";
   TokenNames[KDO] := "KDO";
   TokenNames[KELSE] := "KELSE";
   TokenNames[KELSIF] := "KELSIF";
   TokenNames[KFALSE] := "KFALSE";
   TokenNames[KFOR] := "KFOR";
   TokenNames[KIF] := "KIF";
   TokenNames[KIMPORT] := "KIMPORT";
   TokenNames[KIN] := "KIN";
   TokenNames[KIS] := "KIS";
   TokenNames[KMOD] := "KMOD";
   TokenNames[KMODULE] := "KMODULE";
   TokenNames[KNIL] := "KNIL";
   TokenNames[KOF] := "KOF";
   TokenNames[KOR] := "KOR";
   TokenNames[KPOINTER] := "KPOINTER";
   TokenNames[KPROCEDURE] := "KPROCEDURE";
   TokenNames[KRECORD] := "KRECORD";
   TokenNames[KREPEAT] := "KREPEAT";
   TokenNames[KRETURN] := "KRETURN";
   TokenNames[KTHEN] := "KTHEN";
   TokenNames[KTO] := "KTO";
   TokenNames[KTRUE] := "KTRUE";
   TokenNames[KTYPE] := "KTYPE";
   TokenNames[KUNTIL] := "KUNTIL";
   TokenNames[KVAR] := "KVAR";
   TokenNames[KWHILE] := "KWHILE"
END Scanner.


