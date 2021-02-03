MODULE TestRender;
IMPORT
   Ast, Dbg, In, P:=Parser, Render, Strings;

PROCEDURE TestWrite(fn, outf: ARRAY OF CHAR);
VAR p: P.T;
    ast: Ast.T;
BEGIN
   (*test*)
   p := P.NewFromFile(fn);
   ast := P.ParseModule(p);
   Render.Init(outf, p.scan);
   Render.WriteTree(ast);
   Render.Deinit()
END TestWrite;

PROCEDURE BaseName(path: ARRAY OF CHAR; VAR dest: ARRAY OF CHAR);
VAR i, plen:INTEGER;
BEGIN
   plen := Strings.Length(path);
   i := plen-1;
   WHILE (i >= 0) & (path[i] # "/") DO
      DEC(i)
   END;
   IF (i < 0) OR (path[i] = "/") THEN INC(i) END;
   Strings.Extract(path, i, plen - i, dest)
END BaseName;

(* Stdin has an input file per line. Working around not
   having command line access. *)
PROCEDURE Test();
VAR fbuf, obuf, bname: ARRAY 1024 OF CHAR;
BEGIN
   REPEAT
      In.Line(fbuf);
      IF fbuf[0] # 0X THEN
         BaseName(fbuf, bname);
         ASSERT(bname[0] # 0X);
         (* We expect to be in the test directory when run, 
            so put things in the testout dir from there. *)
         obuf := "testout/";
         Strings.Append(bname, obuf);
         Dbg.S("Parsing: "); Dbg.S(fbuf); Dbg.Ln;
         TestWrite(fbuf, obuf)
      END
   UNTIL fbuf[0] = 0X;
   Dbg.S("ALL DONE."); Dbg.Ln
END Test;

BEGIN
   Test()
END TestRender.

