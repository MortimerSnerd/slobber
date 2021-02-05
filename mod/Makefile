EXES=TestScanner TestParser TestRender TestSymtab
all: $(EXES)

TestScanner: TestScanner.mod Scanner.mod
	CFLAGS=-g obnc TestScanner.mod

TestParser: TestParser.mod Scanner.mod Ast.mod Dbg.mod Parser.mod
	CFLAGS=-g obnc TestParser.mod

TestRender: TestRender.mod Scanner.mod Ast.mod Dbg.mod Parser.mod Render.mod
	CFLAGS=-g obnc TestRender.mod

TestSymtab: TestSymtab.mod Symtab.mod Semcheck.mod Scanner.mod Ast.mod Dbg.mod Parser.mod Types.mod
	CFLAGS=-g obnc TestSymtab.mod

clean:
	rm -rf .obnc $(EXES)
