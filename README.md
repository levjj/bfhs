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

Brainfuck has no literals, therefore parsing usually involves a simple match
operation followed by the value to return. In order to make this easier to
implement with Parsec, the following operator can be used:

```haskell
($>) :: Functor f => f a -> b -> f b
($>) = flip $ fmap . const
```

The second helper function combines IO and a state transformation of the
current memory cell. The supplied function `f` will be call with the current
cell and returns the new cell value as IO monad which will be lifted to be part
of the overall state transformation.

```haskell
withCurrent :: (Char -> IO Char) -> BFThunk
withCurrent f = do (left, cur, right) <- get
                   cur' <- liftIO (f cur)
                   put (left, cur', right)
```

The most important function is actually a Parsec monad which takes care of
parsing and returns instances of `BFThunk`, i.e. it returns state
transformation monads that use IO. The operations "+", "-", "." and "," can be
expressed with the `withCurrent` helper function. The operations ">" and "<"
simple change the state by shifting the current cell either to the left or to
the right. The loop operator "[" benefits from parsing with Parsec because it
matches the brackets "[" and "]" and `many1 bf` then simply returns a list of
BFThunk monads. The loop header then simply checks for the current memory value
and if it is something other than 0, it returns a recursive sequence of state
transformations consisting of the loop body and the loop header again.

```haskell
bf :: Parsec String () BFThunk
bf =  (noneOf "+-.,><[]" $> return ())
  <|> (char '+' $> withCurrent (return . chr . succ . ord))
  <|> (char '-' $> withCurrent (return . chr . pred . ord))
  <|> (char '.' $> withCurrent (\c -> putChar c >> return c))
  <|> (char ',' $> withCurrent (\_ -> getChar))
  <|> (char '>' $> (get >>= \(l, c, r:rs) -> put (c:l, r, rs)))
  <|> (char '<' $> (get >>= \(l:ls, c, r) -> put (ls, l, c:r)))
  <|> do char '['
         body <- many1 bf
         char ']'
         let loop = do (_,c,_) <- get
                       case c of '\0' -> return ()
                                 _    -> sequence_ (body ++ [loop])
         return loop
```

The last 5 lines are implementing the `main` function which expects the
Brainfuck source file name as command line argument and parses it using the
`bf` function. If the parsing was successful then `prog` is a list of `BFThunk`
state transformation monads. Calling `evalStateT` with the initial state will
then evaluate the Brainfuck program which might involve IO.

```haskell
main :: IO ()
main = do (file:_) <- getArgs
          code <- readFile file
          case parse (many1 bf) "" code of
            Left err -> print $ "Error while parsing: " ++ show err
            Right prog -> evalStateT (sequence_ prog) ([], '\0', repeat '\0')
```

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
