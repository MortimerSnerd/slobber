MODULE TestScanner;
IMPORT Out, Lex:=Scanner;
CONST verbose = FALSE;
VAR
   scan: Lex.T;
   t: Lex.TokKind;
   testpos: INTEGER;

PROCEDURE nxt();
VAR op: INTEGER;
BEGIN
   op := scan.pos;
   t := Lex.Next(scan);
   Out.String("Next: "); Out.Int(t, 0); 
   Out.String(" @ "); Out.Int(op, 0); Out.Ln
END nxt;

BEGIN
   Out.String("Ho ho"); Out.Ln;
   Lex.InitFromString(scan, "barf12x 12 12X 12H 12.3E4 (* holy (* crap *)*)  +<=#..IF WHILE <= >= 1..2 INTEGER");
   nxt(); ASSERT(t = Lex.Id);
   nxt(); ASSERT(t = Lex.ConstInt);
   nxt(); ASSERT(t = Lex.ConstHexString);
   nxt(); ASSERT(t = Lex.ConstHex);
   nxt(); ASSERT(t = Lex.ConstReal);
   nxt(); ASSERT(t = Lex.PLUS);
   nxt(); ASSERT(t = Lex.LTE);
   nxt(); ASSERT(t = Lex.OCTOTHORPE);
   nxt(); ASSERT(t = Lex.DOTDOT);
   nxt(); ASSERT(t = Lex.KIF);
   nxt(); ASSERT(t = Lex.KWHILE);
   nxt(); ASSERT(t = Lex.LTE);
   nxt(); ASSERT(t = Lex.GTE);
   nxt(); ASSERT(t = Lex.ConstInt);
   nxt(); ASSERT(t = Lex.DOTDOT);
   nxt(); ASSERT(t = Lex.ConstInt);
   nxt(); ASSERT(t = Lex.Id);
   nxt(); ASSERT(t = Lex.EOF);

   Out.String("Read Scanner.mod"); Out.Ln;
   IF Lex.InitFromFile(scan, "Scanner.mod") THEN
      Out.String("  read bytes: "); Out.Int(scan.len, 0); Out.Ln;
      REPEAT
         testpos := scan.pos;
         t := Lex.Next(scan);
         IF t = Lex.GARBAGE THEN
            Out.String("   GARBAGE AT "); Out.Int(testpos, 0); Out.Ln
         ELSIF verbose THEN
            Out.String(" ");Out.Int(testpos, 0); Out.String(":"); Out.Int(t, 0)
         END
      UNTIL t = Lex.EOF;
      Out.Ln; Out.String("   all file tokens read.")
   ELSE
      Out.String("Can't open file?"); Out.Ln
   END;
   ASSERT(scan.line = 464);
   Out.String("Done"); Out.Ln
END TestScanner.

   
