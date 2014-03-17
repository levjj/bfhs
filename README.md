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

The state of the interpreter is defined as two memory arrays, one for all cells
left from the current cell and one for all cells right of and including the
current cell.

    data BFState = BFState {pointer :: Int} deriving (Show)

The interpreter itself is written using the [The StateT monad transformer ](http://hackage.haskell.org/package/transformers-0.3.0.0/docs/Control-Monad-Trans-State-Lazy.html#g:2) which provides many
useful functions for dealing with both IO and State transformations. This makes
the implementation extremely concise (less than 30 lines in total).

    type BFProg = StateT BFState IO ()

