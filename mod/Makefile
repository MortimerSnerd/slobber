EXES=TestScanner TestParser TestRender TestSymtab MkSymtab DumpAst Compile TestStringTab
.PHONY: clean TestScanner TestParser TestRender TestSymtab MkSymtab DumpAst Compile TestStringTab
all: $(EXES)

TestScanner: 
	CFLAGS=-g obnc TestScanner.mod

TestParser: 
	CFLAGS=-g obnc TestParser.mod

TestRender: 
	CFLAGS=-g obnc TestRender.mod

TestSymtab: 
	CFLAGS=-g obnc TestSymtab.mod

MkSymtab:
	CFLAGS=-g obnc MkSymtab.mod

DumpAst: 
	CFLAGS=-g obnc DumpAst.mod

Compile: 
	CFLAGS=-g obnc Compile.mod

TestStringTab: 
	CFLAGS=-g obnc TestStringTab.mod

clean:
	rm -rf .obnc $(EXES) *.slo
