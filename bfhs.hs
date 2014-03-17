import Data.Char (chr,ord)
import System.Environment (getArgs)
import Control.Monad.IO.Class
import Control.Monad.Trans.State

type BFState = ([Char], Char, [Char])

type BFProg = StateT BFState IO ()

withCurrent :: (Char -> IO Char) -> BFProg
withCurrent f = do (l, c, r) <- get
                   c' <- liftIO (f c)
                   put (l, c', r)

bf :: Char -> BFProg
bf '+' = withCurrent $ return . chr . succ . ord
bf '-' = withCurrent $ return . chr . pred . ord
bf '.' = withCurrent $ \s -> putChar s >> return s
bf ',' = withCurrent $ \_ -> getChar
bf '<' = get >>= \(l:ls, c, r) -> put (ls, l, c:r)
bf '>' = get >>= \(l, c, r:rs) -> put (c:l, r, rs)
bf  _  = return ()

main :: IO ()
main = do (file:_) <- getArgs
          code <- readFile file
          evalStateT (mapM_ bf code) ([], '\0', repeat '\0')
