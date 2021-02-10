EXES=TestScanner TestParser TestRender TestSymtab MkSymtab
.PHONY: clean TestScanner TestParser TestRender TestSymtab MkSymtab
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

clean:
	rm -rf .obnc $(EXES) *.slo
