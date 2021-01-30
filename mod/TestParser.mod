MODULE TestParser;
IMPORT Ast, Dbg, Lex := Scanner, Par := Parser;
VAR 
    q: Ast.Branch;
    tok: Lex.Token;
    par: Par.T;
    root: Ast.T;
    buf: ARRAY 256 OF CHAR;

BEGIN
   buf := "ASSERT(Lex.InitFromFile(p.scan, fname))";
   par := Par.NewFromString(buf);
   root := Par.ParseStatement(par);
   root.ops.toStr(root, par.scan.buf, 0);
   Dbg.Ln;

   par := Par.NewFromFile("Parser.mod");
   root := Par.ParseModule(par);
   root.ops.toStr(root, par.scan.buf, 0);
   Dbg.Ln;   

   Dbg.S("Done."); Dbg.Ln
END TestParser.
