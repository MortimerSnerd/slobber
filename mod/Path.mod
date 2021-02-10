(* Path type.  Directories have trailing / 
   This manipulates path names, it doesn't provide
   an interface to the filesystem. *)
MODULE Path;
IMPORT Strings;
TYPE
   T* = RECORD
      str*: ARRAY 1024 OF CHAR;
      len*: INTEGER
   END;

PROCEDURE Zero*(VAR p: T);
BEGIN
   p.len := 0;
   p.str[0] := 0X
END Zero;

PROCEDURE Update*(VAR p: T);
BEGIN
   p.len := Strings.Length(p.str)
END Update;

PROCEDURE FromZ*(VAR p: T; c: ARRAY OF CHAR);
VAR i: INTEGER;
BEGIN
   p.len := Strings.Length(c);
   FOR i := 0 TO p.len-1 DO
      p.str[i] := c[i]
   END;
   p.str[p.len] := 0X
END FromZ;

PROCEDURE FindLast(p: T; c: CHAR): INTEGER;
VAR rv, i: INTEGER;
BEGIN
   rv := -1;
   i := p.len-1;
   WHILE (rv < 0) & (i >= 0) DO
      IF p.str[i] = c THEN
         rv := i
      ELSE
         DEC(i)
      END
   END
   RETURN rv
END FindLast;

(* Extracts the basename of 'a' and copies it to `dest` *)
PROCEDURE Basename*(a: T; VAR dest: T);
VAR i, pos: INTEGER;
BEGIN
   pos := FindLast(a, "/");
   IF pos >= 0 THEN
      i := 0;
      INC(pos);
      dest.len := a.len - pos;
      WHILE pos <= a.len DO
         dest.str[i] := a.str[pos];
         INC(i); INC(pos)
      END;
   ELSE
      dest.len := 0
   END
END Basename;

(* Strips the file extension off of the given path, if any. *)
PROCEDURE StripExt*(VAR p: T);
VAR i, least: INTEGER;
    found: BOOLEAN;
BEGIN
   least := FindLast(p, "/") + 1;
   i := p.len-1;
   found := FALSE;
   WHILE ~found & (i >= least) DO
      IF p.str[i] = "." THEN
         p.len := i;
         p.str[i] := 0X;
         found := TRUE;
      ELSE
         DEC(i)
      END
   END
END StripExt;

(* Assert that this path is a directory *)
PROCEDURE AssertDir*(VAR p: T);
BEGIN
   IF p.str[p.len-1] # "/" THEN
      ASSERT(p.len < LEN(p.str)-1);
      p.str[p.len] := "/"; INC(p.len);
      p.str[p.len] := 0X
   END
END AssertDir;
     
PROCEDURE Copy*(src: T; VAR dest: T);
VAR i: INTEGER;
BEGIN
   dest.len := src.len;
   FOR i := 0 TO src.len DO
      dest.str[i] := src.str[i]
   END
END Copy;

PROCEDURE Append*(VAR p: T; s: ARRAY OF CHAR);
VAR i, slen: INTEGER;
BEGIN
   slen := Strings.Length(s);
   ASSERT((slen + p.len) < LEN(p.str));
   FOR i := 0 TO slen DO
      p.str[i+p.len] := s[i]
   END;
   p.len := p.len + slen
END Append;

BEGIN
END Path.
