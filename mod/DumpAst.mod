(* Command for debugging parsing *)
MODULE DumpAst;
IMPORT Ast, Dbg, Par:=Parser, Lex:=Scanner, Args:=extArgs;

PROCEDURE DoIt();
VAR par: Par.T;
    root: Ast.T;
    pos: Ast.SourcePos;
    fname: ARRAY 1024 OF CHAR;
    i, res: INTEGER;
BEGIN
   FOR i := 0 TO Args.count-1 DO
      Args.Get(i, fname, res);
      ASSERT(res = 0);
      Dbg.S("FILE: "); Dbg.S(fname); Dbg.Ln;
      par := Par.NewFromFile(fname);
      root := Par.ParseModule(par, FALSE);
      root.ops.toStr(root, par.scan.buf, 0);
      Dbg.Ln;   
   END
END DoIt;

BEGIN
   DoIt
END DumpAst.
