import Data.Char (chr,ord)
import System.Environment (getArgs)
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Text.Parsec
import Text.Parsec.String (Parser)

-- Brainfuck State
type BFState = ([Char], Char, [Char])

-- A Brainfuck program is simply a State Transformation Monad
type BFThunk = StateT BFState IO ()

-- Helper function to make the parsing easier
($>) :: Functor f => f a -> b -> f b
($>) = flip $ fmap . const

-- Helper function to do something with the current cell (inc. IO)
withCurrent :: (Char -> IO Char) -> BFThunk
withCurrent f = do (l, c, r) <- get
                   c' <- liftIO (f c)
                   put (l, c', r)

-- Brainfuck parser and thunk generator
bf :: Parser BFThunk
bf =  (noneOf "+-.,><[]" $> return ())
  <|> (char '+' $> withCurrent (return . chr . succ . ord))
  <|> (char '-' $> withCurrent (return . chr . pred . ord))
  <|> (char '.' $> withCurrent (\s -> putChar s >> return s))
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
