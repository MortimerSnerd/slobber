
(* A segment in the compiler output, such as data, or code. 
   Not really that great for bss. Associates names with segment offsets.
   Hardcoded little-endian since I don't expect this to be more than
   one platform. *)
MODULE Segment;
IMPORT
   Bit:=Bitwise, StringTab, Target, Types;

CONST
   ChunkSz = 8192;
   NamesPerGrp = 256;
   MaxNameSz* = 64;
   
   (* Constants for Reloc.kind *)
   rk64=1;

TYPE
   ChunkPtr = POINTER TO Chunk;
   Chunk = RECORD
      offset: INTEGER;
      data: ARRAY ChunkSz OF BYTE;
      next: ChunkPtr
   END;

   RelocPtr = POINTER TO Reloc;
   Reloc = RECORD
      (* Records a location in the segment that will need to be 
         relocated when the segment is placed at an absolute address, or
         relative to other segments *)
      kind: INTEGER;  (* rk* constants *)
      offset, pointsTo: INTEGER;
      next: RelocPtr
   END;
      
   NameGroupPtr = POINTER TO NameGroup;
   NameGroup = RECORD
      (* Block of associated names *)
      count: INTEGER;
      names: ARRAY NamesPerGrp OF StringTab.SymPtr;
      offsets: ARRAY NamesPerGrp OF INTEGER;
      next: NameGroupPtr
   END;

   T* = POINTER TO RECORD
      (* Segment containing data, and names referencing offsets
         into the segment. *)
      first, last: ChunkPtr;
      sz: INTEGER;
      names: NameGroupPtr;
      relocs: RelocPtr;
      baseoffset: INTEGER
         (* # 0 once this segment has been relocated. *)
   END;

PROCEDURE MkSegment*(): T;
VAR rv: T;
BEGIN
   NEW(rv);
   NEW(rv.first);
   rv.first.next := NIL;
   rv.last := rv.first;
   rv.sz := 0;
   rv.names := NIL;
   rv.relocs := NIL;
   rv.baseoffset := 0;
   RETURN rv
END MkSegment;

PROCEDURE Align(seg: T; alignment: INTEGER);
   (* Advances the data offset if necessary to get the given
      alignment *)
BEGIN
   seg.sz := Target.Align(seg.sz, alignment)
END Align;

PROCEDURE AppendChunk(seg: T);
   (* Extends the data by another chunk *)
VAR ch: ChunkPtr;
BEGIN
   NEW(ch);
   ch.next := NIL;
   ch.offset := seg.last.offset + ChunkSz;
   seg.last.next := ch;
   seg.last := ch
END AppendChunk;

PROCEDURE Outb(seg: T; n: INTEGER);
   (* Doing this every byte is dire, but keeping it 
      simple for now *)
VAR si: INTEGER;
BEGIN
   si := si MOD ChunkSz;
   WHILE seg.sz >= (seg.last.offset+ChunkSz) DO
      AppendChunk(seg);
      si := 0;
   END;
   seg.last.data[si] := n;
   INC(seg.sz)
END Outb;

PROCEDURE I32*(seg: T; n: INTEGER);
   (* Write out 32 bit integer at current location. Does not
      force alignment. *)
BEGIN
   Outb(seg, Bit.And(0FFH, n)); n := ROR(n, 8);
   Outb(seg, Bit.And(0FFH, n)); n := ROR(n, 8);
   Outb(seg, Bit.And(0FFH, n)); n := ROR(n, 8);
   Outb(seg, Bit.And(0FFH, n));
   seg.sz := seg.sz + 4
END I32;

PROCEDURE Ptr*(seg: T; offset: INTEGER);
   (* Write a absolute pointer to an offset within the segment. 
      Adds a relocation record so the pointer can be fixed up. *)
VAR rp: RelocPtr;
BEGIN
   NEW(rp);
   rp.kind := rk64;
   rp.offset := seg.sz;
   rp.pointsTo := offset;
   rp.next := seg.relocs;
   seg.relocs := rp;
   I32(seg, rp.offset); I32(seg, 0);
END Ptr;

PROCEDURE S*(seg: T; s: ARRAY OF CHAR);
   (* Writes the entire array out *)
VAR i: INTEGER;
BEGIN
   FOR i := 0 TO LEN(s)-1 DO
      Outb(seg, ORD(s[i]))
   END;
END S;

PROCEDURE Tag*(seg: T; name: StringTab.SymPtr);
   (* Add a name and link it to an offset.  The offset 
      will be updated as things are relocated. *)
VAR ng: NameGroupPtr;
BEGIN
   ng := seg.names;
   IF ng.count >= NamesPerGrp THEN
      NEW(ng);
      ng.count := 0;
      ng.next := seg.names;
      seg.names := ng;
   END;
   ng.names[ng.count] := name;
   ng.offsets[ng.count] := seg.sz;
   INC(ng.count);
END Tag;

(* TODO: will need to reference data segment addresses from code.  How to
         how to reloc those and other references? *)


(* Compiling
       Previously compiled mod, loaded from slo file: 
            +---------
            | Module
            | rodata segment
            | data segment
            | bss (mod vars)
            | code segment (assembly language)
            +-------------

      Generating Il
          GenerateState
             +----------
             | Module
             | rodata segment
             | data segment
             | bss
             | il segment
             +----------

     Before Il is generated, need to generate the rodata segment, and 
     the bss segment.  (bss doesn't really work well as a segment, what we
     really need is the offset tagging mechanism as a separate piece which 
     we can use by itself for bss, and Segment.T can use for initialized
     segments. 

     Il references:
         Two cases:
            VarNameOpt refrerences to local and external mod vars

            (unimplemented) SegmentRef that refers to a Segment.Tag()
               in a segment.  (Another ValueOpt child)

         These two cases are closer than they might appear.  VarNameOpt
         is a reference to a Module var, and will have a TypeSym that 
         allows us to generate a qualified name for the var that can be used
         to look up the segment Tag.  The SegmentRef case is for 
         synthetic vars created for the runtime that don't have a 
         TypeSym, like the RUNTIME.VTable for records.  
      
         It would not be that hard to create TypeSyms for the synthetic
         vars, and just keep this down to one case.  We just need to be
         careful not to link them in anywhere where they'd show up in 
         a Module's TypeSym lists.
        



     When Il is converted to assembly to module, it will need to 
     have the relocations for the VarNameOpt.  This implies that these
     references will need to be turned into Segment relocations as the
     machine language is generated.  The Reloc currently implemented is
     not sufficient, as it's only within a segment.  

     So if the code segment loads the address of a VTable pointer, 
     the code segment will need an "External" reloc record for the
     code segment that says "this N byte location in the code segment
     points to 'Module.RecordName/VTable' in the rodata segment".  

     Finally, when everything is linked, to either run the code
     immediately, or make an executable, the data segments are 
     assembled and relocated first, and then code.  AFAIK, there will
     be no references to code addresses in the data segments.  
     


*)

END Segment.
