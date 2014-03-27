bfhs: Brainfuck interpreter in Haskell
======================================

[Brainfuck](http://en.wikipedia.org/wiki/Brainfuck) is an esoteric programming
language which mimics a one tape turing machine and has a very simple grammar:

 - `"+"`  increments the value in the current cell
 - `"-"`  decrements the value in the current cell
 - `"."`  writes the value in current cell to standard output
 - `","`  reads a value from standard input and stores it in the current cell
 - `"["`  while the value in the current cell is non-zero, proceed to the next
        cell, otherwise jump to the instruction after the matching `"]"`
 - `"]"`  jump back to the matching `"["` instruction

Every other character in the source code is treated as comment.

Interpreter
-----------

The state of the interpreter consists of all memory cells left from the current
cell, the current cell, and all cells to the right of the current cell.

```haskell
type BFState = ([Char], Char, [Char])
```

In contrast to other Brainfuck interpreters, the source code is parsed first
using the amazing parser generator [Parsec](http://legacy.cs.uu.nl/daan/parsec.html).
This makes loops trivial to implement.

The parser does not create an abstract syntax tree. Instead, it created thunks
which are instances of the [The StateT monad transformer ](http://hackage.haskell.org/package/transformers-0.3.0.0/docs/Control-Monad-Trans-State-Lazy.html#g:2).

```haskell
type BFThunk = StateT BFState IO ()
```

This makes it possible to deal with both IO and State transformations at the
same time and makes the implementation extremely concise (45 lines including
comments).

Compiling, Running and Testing
------------------------------

    $ make
    $ ./bfhs a.bf
    $ ./bfhs hello.bf
    $ ./bfhs squares.bf

Acknowledgements
----------------

The blog post about [Haskell](http://haupz.blogspot.com/2012/10/haskell.html)
by Michael Haupt inspired me to also implement a Brainfuck interpreter in Haskell.
His implementation does not use the IO monad and as such the output of the
interpreter Brainfuck program is only visible after the program terminates.
However, which implementation is easier to understand probably depends on the
reader.
