(* Module that handles reading and writing binary files.
   Doing the simplest thing possible, it wouldn't surprise
   me if this ends up being slow.  Just using whatever Files 
   gives us, this may change so it's set to known endianess.  
   *)
MODULE BinReader;
IMPORT
   Files;

TYPE
   T* = RECORD
      hand: Files.File;
      rd: Files.Rider
   END;

(* Opens up a binary file to be written *)
PROCEDURE Init*(VAR w: T; fname: ARRAY OF CHAR): BOOLEAN;
BEGIN
   w.hand := Files.Old(fname);
   IF w.hand # NIL THEN
      Files.Set(w.rd, w.hand, 0)
   END;
   RETURN w.hand # NIL
END Init;

(* Should be called when finshed with a BinReader/BinWriter to clean up
   resources *)
PROCEDURE Finish*(VAR w: T);
BEGIN
   IF w.hand # NIL THEN
      Files.Close(w.hand);
      w.hand := NIL
   END
END Finish;

(* NB not checking the range here, because we assume the 
   Oberon implementation INTEGER is 32 bits. *)
PROCEDURE I32*(VAR w: T; VAR rv: INTEGER); 
BEGIN
   Files.ReadInt(w.rd, rv);
END I32;   

PROCEDURE I8*(VAR w: T; VAR rv: INTEGER); 
BEGIN
   Files.ReadInt(w.rd, rv);
   ASSERT((rv >= -128) & (rv <= 127));
END I8;

PROCEDURE String*(VAR w: T; VAR s: ARRAY OF CHAR);
BEGIN
   Files.ReadString(w.rd, s)
END String;

PROCEDURE Bool*(VAR w: T; VAR rv: BOOLEAN);
VAR i: INTEGER;
BEGIN
   I8(w, i);
   rv := i # 0
END Bool;

PROCEDURE F32*(VAR r: T; VAR rv: REAL);
BEGIN
   Files.ReadReal(r.rd, rv)
END F32;

PROCEDURE U8*(VAR r: T; VAR b: BYTE);
BEGIN
   Files.Read(r.rd, b)
END U8;

(* Helper for when you need an optional element,
   so you can record a bool that tells whether the option
   is there while testing the option at the same time:
      Ex:
          IF Cond(w, x # NIL) THEN ReadPointerVal(w, x) END;
*)
PROCEDURE Cond*(VAR w: T): BOOLEAN;
VAR rv: BOOLEAN;
BEGIN
   Bool(w, rv);
   RETURN rv
END Cond;
      
BEGIN
END BinReader.
