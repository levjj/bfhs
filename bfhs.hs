import Data.Char (chr,ord)
import System.Environment (getArgs)
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Text.Parsec
import Text.Parsec.String (Parser)

data BFProg = Inc | Dec | PutC | GetC | Next | Prev | Loop [BFProg] | Skip

-- Brainfuck Parser
bfParser :: Parser BFProg
bfParser =  (noneOf "+-.,><[]" >> return Skip)
        <|> (char '+' >> return Inc)
        <|> (char '-' >> return Dec)
        <|> (char '.' >> return PutC)
        <|> (char ',' >> return GetC)
        <|> (char '>' >> return Next)
        <|> (char '<' >> return Prev)
        <|> do char '['
               s <- many1 bfParser
               char ']'
               return $ Loop s

type BFState = ([Char], Char, [Char])

type BFThread = StateT BFState IO ()

withCurrent :: (Char -> IO Char) -> BFThread
withCurrent f = do (l, c, r) <- get
                   c' <- liftIO (f c)
                   put (l, c', r)

bf :: BFProg -> BFThread
bf Skip = return ()
bf Inc = withCurrent $ return . chr . succ . ord
bf Dec = withCurrent $ return . chr . pred . ord
bf PutC = withCurrent $ \s -> putChar s >> return s
bf GetC = withCurrent $ \_ -> getChar
bf Next = get >>= \(l, c, r:rs) -> put (c:l, r, rs)
bf Prev = get >>= \(l:ls, c, r) -> put (ls, l, c:r)
bf (Loop body) = do (_,c,_) <- get
                    case c of
                      '\0' -> return ()
                      _    -> mapM_ bf (body ++ [Loop body])

main :: IO ()
main = do (file:_) <- getArgs
          code <- readFile file
          case parse (many1 bfParser) "" code of
            Left err -> print $ "Error while parsing: " ++ show err
            Right prog -> evalStateT (mapM_ bf prog) ([], '\0', repeat '\0')
