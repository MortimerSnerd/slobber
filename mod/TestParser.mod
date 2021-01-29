MODULE TestParser;
IMPORT Ast, Dbg, Lex := Scanner, Par := Parser;
VAR 
    q: Ast.Branch;
    tok: Lex.Token;
    par: Par.T;
    root: Ast.T;
    buf: ARRAY 256 OF CHAR;

BEGIN
   buf := "Holy.Crap";
   Par.InitFromString(par, buf);
   root := Par.ParseQualIdent(par);
   root.ops.toStr(root, par.scan.buf, 0);
   Dbg.Ln;

   buf := "PROCEDURE(a,b: INTEGER; VAR c: ARRAY OF REAL): Boarf";
   Par.InitFromString(par, buf);
   root := Par.ParseProcedureType(par);
   root.ops.toStr(root, par.scan.buf, 0);
   Dbg.Ln;   

   buf := "MODULE Bones; IMPORT Lex:=Scanner, Strings; CONST Borked*=1; TYPE smedly* = POINTER TO CHAR; END Bones.";
   Par.InitFromString(par, buf);
   root := Par.ParseModule(par);
   root.ops.toStr(root, par.scan.buf, 0);
   Dbg.Ln;   

   Par.InitFromFile(par, "TestParser.mod");
   root := Par.ParseModule(par);
   root.ops.toStr(root, par.scan.buf, 0);
   Dbg.Ln;   

   Dbg.S("Done."); Dbg.Ln
END TestParser.
