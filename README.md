bfhs: Brainfuck interpreter in Haskell
======================================

This interpreter uses the following state monad:

[Brainfuck](http://en.wikipedia.org/wiki/Brainfuck) is an esoteric programming
language which mimics a one tape turing machine and has a very simple grammar:

    `+`  increments the value in the current cell

    `-`  decrements the value in the current cell

    `.`  writes the value in current cell to standard output

    `,`  reads a value from standard input and stores it in the current cell

    `[`  while the value in the current cell is non-zero, proceed to the next
         cell, otherwise jump to the instruction after the matching `]`

    ']'  jump back to the matching `[` instruction


