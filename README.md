This is just a playground for a toy language.

In the beginning, it's going to be a straight version of Oberon-07 that 
can be interpreted.  Probably not super efficiently. Once it's at that stage, 
I can use it as a platform to try out changes to see the effects on the
backend, without dealing with a lot of code generation to get in the way.

## Current status
Parses Oberon into a AST with no semantic checking.  Very little testing
so far, so it's output is probably only slightly better than crapping
in a paper bag.  But it does nicely pretty print a AST.

## TODOS

- expand constant expression evaluator.  It would be nice 
  if it wasn't in symtab, as it's a lot of boilerplate code, 
  but it does need a hook into symtab lookups for constants.

- Add procs to save and load the module symtabs. When we save
  a symtab file for a module, just the public entries get saved.
  Once we can load these, that's how we'll populate the modules
  for imports.

- Add vars section to symtab.

- Add proc decls to symtab.

- Once all the type information is in place, can 
  do semantic and type checking, and fix up any
  function calls that were parsed as type guards.
 

## Notes

### First checkin of scanner and parser in Oberon.

Short version: Restarted parser and scanner in Oberon.  Using 
the obnc 0.16 compiler.  It has a good quality conversion to C, 
and seems to strictly follow the Oberon-07 grammar.  There's a 
Makefile in the mod/ directory that builds simple test harnesses
for the parser and scanner.

Long version: After writing some parsing code in Nim, I had a change of heart, and 
threw out that version and started a version in Oberon itself.  I'd 
already had it in mind to use a subset of Nim to make it fairly simple
to mechanically translate into Oberon.  But why even have that as an
extra step?

This also gives me a pretty good handle on the painful parts of writing
on Oberon.  I don't have hard opinions yet, I need to write more, but:

1) The semicolon placement is fiddly, but makes sense in how they're placed in 
   the grammar.  I don't often have semicolon problems writing new code.  But
   I tend to introduce them when changing existing code.

2) Only being able to return from the end of the function irritated me at
   first.  Hell, it might still irritate me.  You definitely have to add some 
   ephemeral vars sometimes to be able to have a single exit.  But I also 
   found it convenient when I needed to bracket some parse functions with debug 
   Enter/LeaveCtx functions.  Put the LeaveCtx() before the return, and it's 
   always being called.  It makes think differently about equivalents like the 
   Zig/Go/Nim "defer".

3) I can see now why adding a auto "result" var for procedures that have
   return values is a natural addition to languages that have the 
   Pascal/Modula/Oberon heritage.  Especially with the single function exit 
   discipline.  It can be irritating to have to stutter the return type in 
   the VAR section of the procedure right after having to write it out in the
   procedure header.

4) I'm not sure if I miss enums, but I do miss being able to easily get
   their string names.  Had to build tables by hand for Scanner and Ast
   constants for error and debugging messages, because I was about to go crazy
   interpreting "Was expecting 43 but got 5" errors.  If I could just group some
   constants, and tell the compiler to generate a name lookup for those constants
   even without an enum abstraction.

5) I ended up changing some CASE statements to IF/ELSIF chains in the scanner, 
   because there's no longer any ELSE clause for CASE, and even for ASCII, I didn't
   feel like explicitly delineating all of the ranges that I considered an invalid
   char.  With obnc, the program will panic if there's an unhandled case.  Not
   sure about this, but since I can't define smaller sets of numbers to be exhaustive
   about with enums/whatever, there aren't a lot of situations I want to use this with.
   I haven't used the type selection cases, but I assume there, your "default" case
   is a check for a super type that's the last case handled.

6) Only had to do a tiny bit of manual vtable building in Ast right now.  I'll
   definitely need to more when I get to semantic checking.  I strongly suspect
   it will be irritating.  

7) I had forgot/never noticed that most Oberon impls only allow you to create new
   records.  No allocating variable size strings, unless you're talking about some
   different bucket size records you made.  Urf.  I'm definitely going to add more
   general allocation to the System module.

8) Null terminated strings.  They get passed as open array, and they do the opposite
   thing that C does with null terminated strings, where the LEN("ads") = 4 because
   it counts the \0.  Took me a while to realize that when debugging why the scanner's
   keyword checks weren't working. (ie: LEN is always the length of the array, 
   don't assume it's doing a Strings.Length() for you).

It's interesting, it does remind me of C in that it is very "what you see is
what you get".  Nothing more magic than copying records/arrays on assignments.
It sounds like you're supposed to expose more lower level functionality with 
modules, but it seems like it could be a closer to a systems level language
with control over structure layout, a little more allocation control.  


### Parser testing

Feeding the parser more source.  Shook out some bugs.

The EBNF doesn't reflect how most source in the wild 
has a semicolon between the last statement in a sequence
and a closing RETURN statement.  I suspect this is because
there isn't really a lookahead notation in EBNF that would allow
StatementSequence to say "it's ok for there to be no
statement after a semicolon if the next token is a RETURN".

I work around this by just doing the check in StatementSequence and
adding a comment that points out the difference to the EBNF for 
that function. 
   
I thought I was pretty good at dealing with the semicolon 
placement. Feeding more of my source to the parser tells a 
different story.  obnc is more forgiving than I thought. 
Probably so programmers wouldn't set it on fire and throw it 
off a building.  

For testing, looking at the generated trees doesn't get far.
Writing a quick renderer that will convert an AST back to 
source, so I can compile and test to verify that it's at
least accurate enough to not mangle anything.

### 1/30/21
That actually worked better than I expected.  I added a
test script that parses all of our source, renders it out
into a test directory, and then builds it.  That shook out a
lot of cases I missed or flubbed in the tree building.
And then it does another pass using the executable compiled
from the rendered code, and checks to see if the executable
produced by this pass matches the previous one.

The obvious downside is this only exercises the constructs that
I used in the source code.  I will need to pull some oberon from
other sources and do some parse and render passes on those as well
to make sure I'm not missing anything else.  

I missed some cases in the renderer that an exhaustive matching
case would have helped me catch without an exception.


### 2/2/21

Building up the symbol table code is slow .  There are a lot of 
moving parts that need to be in place just to be able to 
parse the types.

For example:

    Store* = ARRAY NumEnts*2, Han.NumSlices OF SomeRec;

Just to convert this type, we need to be able to lookup 
previously defined constants, constants defined in other
modules that we imported, and we need to be able to evaluate
constant expressions so we know the exact array size.

But going well enough.  I have an incomplete constant
expression evaluator, and there's no such thing as 
importing another module, but I can process the 
const and type sections of a module otherwise. Including
the one special case so POINTER TO X can be declared
before X.

Vars and procedure decls are left.  Vars are simple.
For procedures, this pass just fills out the symbol
tables with the declarations.  After that, there's
enough information to do semantic checking on the 
procedure and module bodies.

### 2/4/21

We build the symbol table now, minus import loading.
Added a bare minimum semcheck module that is using
the type information to fix up ambiguities in the
AST.

Doing the symbol table separate and fixing up the tree 
later is a lot more work than doing it at the same time
of the parse.  I don't regret it. It makes loosening the
declaration order dependencies easier later.  But it does 
make me appreciate the succinctness of the Oberon
compiler from the N. Wirth compiler all the more.

Need to do more cleanup, and actually start doing
type and semantic checking.

