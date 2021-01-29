all: TestScanner TestParser

TestScanner: TestScanner.mod Scanner.mod
	CFLAGS=-g obnc TestScanner.mod

TestParser: TestParser.mod Scanner.mod Ast.mod Dbg.mod Parser.mod
	CFLAGS=-g obnc TestParser.mod

clean:
	rm -rf .obnc TestScanner TestParser
