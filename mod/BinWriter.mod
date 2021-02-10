(* Module that handles reading and writing binary files.
   Doing the simplest thing possible, it wouldn't surprise
   me if this ends up being slow.  Just using whatever Files 
   gives us, this may change so it's set to known endianess.  
   *)
MODULE BinWriter;
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
   w.hand := Files.New(fname);
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
      Files.Register(w.hand);
      Files.Close(w.hand);
      w.hand := NIL
   END
END Finish;

(* NB not checking the range here, because we assume the 
   Oberon implementation INTEGER is 32 bits. *)
PROCEDURE I32*(VAR w: T; i: INTEGER); 
BEGIN
   Files.WriteInt(w.rd, i)
END I32;   

PROCEDURE I8*(VAR w: T; i: INTEGER); 
BEGIN
   ASSERT((i >= -128) & (i <= 127));
   Files.WriteInt(w.rd, i)
END I8;

PROCEDURE F32*(VAR w: T; r: REAL);
BEGIN
   Files.WriteReal(w.rd, r)
END F32;

PROCEDURE U8*(VAR w: T; b: BYTE);
BEGIN
   Files.Write(w.rd, b)
END U8;

PROCEDURE String*(VAR w: T; s: ARRAY OF CHAR);
BEGIN
   Files.WriteString(w.rd, s)
END String;

PROCEDURE Bool*(VAR w: T; b: BOOLEAN);
BEGIN
   IF b THEN I8(w, 1) ELSE I8(w, 0) END;
END Bool;


(* Helper for when you need an optional element,
   so you can record a bool that tells whether the option
   is there while testing the option at the same time:
      Ex:
          IF Cond(w, x # NIL) THEN WritePointerVal(w, x) END;
*)
PROCEDURE Cond*(VAR w: T; b: BOOLEAN): BOOLEAN;
BEGIN
   Bool(w, b);
   RETURN b
END Cond;
      
BEGIN
END BinWriter.
