(* Module for debug output *)
MODULE Dbg;
IMPORT
   Out;

CONST IndMultiple = 4;

PROCEDURE I*(x: INTEGER); 
BEGIN
   Out.Int(x, 0)
END I;


PROCEDURE S*(x: ARRAY OF CHAR); 
BEGIN
   Out.String(x)
END S;

PROCEDURE Ln*();
BEGIN
   Out.Ln
END Ln;

PROCEDURE Ind*(n: INTEGER);
BEGIN
   n := n * IndMultiple;
   WHILE n > 0 DO
      Out.Char(" ");
      DEC(n)
   END
END Ind;

PROCEDURE Slice*(src: ARRAY OF CHAR; pos, len: INTEGER);
VAR i: INTEGER;
BEGIN
   i := 0;
   WHILE i < len DO
      Out.Char(src[i+pos]);
      INC(i)
   END
END Slice;

END Dbg.
