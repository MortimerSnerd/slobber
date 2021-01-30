EXES=TestScanner TestParser TestRender
all: $(EXES)

TestScanner: TestScanner.mod Scanner.mod
	CFLAGS=-g obnc TestScanner.mod

TestParser: TestParser.mod Scanner.mod Ast.mod Dbg.mod Parser.mod
	CFLAGS=-g obnc TestParser.mod

TestRender: TestRender.mod Scanner.mod Ast.mod Dbg.mod Parser.mod Render.mod
	CFLAGS=-g obnc TestRender.mod


clean:
	rm -rf .obnc $(EXES)
