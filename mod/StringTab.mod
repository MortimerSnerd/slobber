(* Allows strings to be interned into "symbols" that 
   is a offset length pair for small strings *)
MODULE StringTab;
IMPORT
   Strings;

CONST
   ChunkSz = 8192;

TYPE
   SymPtr* = POINTER TO Sym;
   Sym* = RECORD
      (* A symbol for a string that has been interned *)
      offset*, len*: INTEGER;
      next: SymPtr
   END;

   ChunkPtr = POINTER TO Chunk;
   Chunk = RECORD
      offset: INTEGER;
      data: ARRAY ChunkSz OF CHAR;
      next: ChunkPtr
   END;

   TablePtr* = POINTER TO Table;
   Table* = RECORD
      sz: INTEGER;
      first, last: ChunkPtr;
      symbols: SymPtr
   END;

PROCEDURE ChunkFor(tab: TablePtr; offset: INTEGER): ChunkPtr;
VAR ch: ChunkPtr;
    found: BOOLEAN;
BEGIN
   ch := tab.first;
   found := FALSE;
   WHILE ~found & (ch # NIL) DO
      IF offset < (ch.offset + ChunkSz) THEN
         found := TRUE
      ELSE
         ch := ch.next
      END
   END;
   RETURN ch
END ChunkFor;

PROCEDURE Eq(tab: TablePtr; sym: SymPtr; str: ARRAY OF CHAR): BOOLEAN;
   (* Assumes String.Length(str) >= sym.len *)
VAR ch: ChunkPtr;
    tabi, stri: INTEGER;
    matching: BOOLEAN;
BEGIN
   ch := ChunkFor(tab, sym.offset);
   tabi := sym.offset - ch.offset;
   stri := 0;
   matching := TRUE;
   WHILE matching & (stri < sym.len) DO
      IF ch.data[tabi] # str[stri] THEN
         matching := FALSE
      END;
      INC(stri); INC(tabi)
   END;
   RETURN matching
END Eq; 

PROCEDURE Find(tab: TablePtr; needle: ARRAY OF CHAR): SymPtr;
VAR found: BOOLEAN;
    nlen: INTEGER;
    sym: SymPtr;
BEGIN
   found := FALSE;
   nlen := Strings.Length(needle);
   sym := tab.symbols;
   WHILE ~found & (sym # NIL) DO
      IF (sym.len = nlen) & Eq(tab, sym, needle) THEN
         found := TRUE
      ELSE
         sym := sym.next
      END
   END;
   RETURN sym
END Find;

PROCEDURE AppendChunk(tab: TablePtr);
   (* Adds another chunk to the end of the chunk list *)
VAR ch: ChunkPtr;
BEGIN
   NEW(ch);
   ch.offset := tab.last.offset + ChunkSz;
   ch.next := NIL;
   tab.last.next := ch;
   tab.last := ch
END AppendChunk;

PROCEDURE MkTable*(): TablePtr;
   (* Create a new Table *)
VAR rv: TablePtr;
BEGIN
   NEW(rv);
   NEW(rv.first);
   rv.first.offset := 0;
   rv.first.next := NIL;
   rv.last := rv.first;
   rv.symbols := NIL;
   RETURN rv
END MkTable;

PROCEDURE Add(tab: TablePtr; str: ARRAY OF CHAR): SymPtr;
   (* Unconditionally adds "str" to the table *)
VAR rv: SymPtr;
    slen, stri, tabi: INTEGER;
    ch: ChunkPtr;
BEGIN
   slen := Strings.Length(str);
   (* For simplicity, don't allow to straddle chunks *)
   ASSERT(slen < LEN(ch.data));
   tabi := tab.sz MOD ChunkSz;
   IF (tabi + slen) >= ChunkSz THEN
      AppendChunk(tab);
      tabi := 0;
      tab.sz := tab.last.offset;
   END;
   ch := tab.last;
   NEW(rv);
   rv.offset := tab.sz;
   rv.len := slen;
   rv.next := tab.symbols;
   tab.symbols := rv;
   FOR stri := 0 TO slen-1 DO
      ch.data[tabi] := str[stri];
      INC(tabi)
   END;
   tab.sz := tab.sz + slen;
   RETURN rv
END Add;   

PROCEDURE SymFor*(tab: TablePtr; str: ARRAY OF CHAR): SymPtr;
   (* Returns the existing symbol for the given string, or
      creates a new symbol if the string isn't matched *)
VAR rv: SymPtr;
BEGIN
   rv := Find(tab, str);
   IF rv = NIL THEN
      rv := Add(tab, str)
   END;
   RETURN rv
END SymFor;

PROCEDURE Extract*(tab: TablePtr; sym: SymPtr; VAR dest: ARRAY OF CHAR);
   (* Extracts a string for a symbol. *)
VAR i, tabi: INTEGER;
    ch: ChunkPtr;
BEGIN
   ASSERT(LEN(dest) >= (sym.len+1));
   ch := ChunkFor(tab, sym.offset);
   tabi := sym.offset MOD ChunkSz;
   FOR i := 0 TO sym.len-1 DO
      dest[i] := ch.data[tabi];
      INC(tabi)
   END;
   dest[i] := 0X
END Extract;

PROCEDURE FirstSym*(tab: TablePtr): SymPtr;
   RETURN tab.symbols
END FirstSym;

PROCEDURE NextSym*(sym: SymPtr): SymPtr;
   RETURN sym.next
END NextSym;

END StringTab.
