This is just a playground for a toy language.

In the beginning, it's going to be a straight version of Oberon-07 that 
can be interpreted.  Probably not super efficiently. Once it's at that stage, 
I can use it as a platform to try out changes to see the effects on the
backend, without dealing with a lot of code generation to get in the way.

## Current status
Still writing the parser, which just generates a straightforward AST
that matches up with the grammar for the most part.  It will probably
cause me pain later when I need to traverse it, and then I'll see
about cleaning it up so there's more re-use and less special cases
needed.

