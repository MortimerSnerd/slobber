(* Module for debug output *)
MODULE Dbg;
IMPORT
   Cvt:=extConvert, Out, Strings;

CONST IndMultiple = 4;

VAR
   col: INTEGER;

PROCEDURE I*(x: INTEGER); 
VAR buf: ARRAY 22 OF CHAR; (* 19 digits in int64 + sign + null *)
    done: BOOLEAN;
BEGIN
   Cvt.IntToString(x, buf, done);
   Out.String(buf);
   col := col + Strings.Length(buf)
END I;


PROCEDURE S*(x: ARRAY OF CHAR); 
BEGIN
   Out.String(x);
   col := col + Strings.Length(x)
END S;

PROCEDURE Ln*();
BEGIN
   Out.Ln;
   col := 0;
END Ln;

PROCEDURE Ind*(n: INTEGER);
BEGIN
   n := n * IndMultiple;
   WHILE n > 0 DO
      Out.Char(" ");
      DEC(n);
      INC(col)
   END
END Ind;

PROCEDURE Slice*(src: ARRAY OF CHAR; pos, len: INTEGER);
VAR i: INTEGER;
BEGIN
   i := 0;
   WHILE i < len DO
      Out.Char(src[i+pos]);
      INC(i);
      INC(col)
   END
END Slice;

PROCEDURE PadTo*(colnum: INTEGER; padChar: CHAR);
   (* Pads output with padChar up to colnum *)
BEGIN
   WHILE col < colnum DO
      Out.Char(padChar);
      INC(col)
   END
END PadTo;

BEGIN
   col := 1;
END Dbg.
