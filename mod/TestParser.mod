MODULE TestParser;
IMPORT Ast, Dbg, Par := Parser;
VAR 
    par: Par.T;
    root: Ast.T;
    buf: ARRAY 256 OF CHAR;
    pos: Ast.SourcePos;

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

   par := Par.NewFromFile("Render.mod");
   root := Par.ParseModule(par);
   root.ops.toStr(root, par.scan.buf, 0);
   Dbg.Ln;   

   Ast.Position(Ast.GetChild(root(Ast.Branch), Ast.ModuleDecls), par.scan, pos);
   Dbg.S("Line/col of decls is "); Dbg.I(pos.line); Dbg.S(" "); Dbg.I(pos.col);Dbg.Ln;

   Dbg.S("Done."); Dbg.Ln
END TestParser.
