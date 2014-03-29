import Data.Char (chr,ord)
import System.Environment (getArgs)
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Text.Parsec

-- Brainfuck State (cells on the left, current cell, cells on the right)
type BFState = ([Char], Char, [Char])

-- A Brainfuck program is simply a State Transformation monad with IO
type BFThunk = StateT BFState IO ()

-- Helper function to make the parsing easier
($>) :: Functor f => f a -> b -> f b
($>) = flip $ fmap . const

-- Helper function to do something with the current cell (including IO)
withCurrent :: (Char -> IO Char) -> BFThunk
withCurrent f = do (left, cur, right) <- get
                   cur' <- liftIO (f cur)
                   put (left, cur', right)

-- Brainfuck parser and thunk generator
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

main :: IO ()
main = do (file:_) <- getArgs
          code <- readFile file
          case parse (many1 bf) "" code of
            Left err -> print $ "Error while parsing: " ++ show err
            Right prog -> evalStateT (sequence_ prog) ([], '\0', repeat '\0')
