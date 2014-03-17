import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.Char (chr,ord)
import System.Environment (getArgs)

data BFState = BFState {left :: [Char],
                        current :: Char,
                        right :: [Char]} deriving (Show)

type BFProg = StateT BFState IO ()

withCurrent :: (Char -> Char) -> BFProg
withCurrent f = modify $ \(BFState l c r) -> BFState l (f c) r

inc :: BFProg
inc = withCurrent (chr . succ . ord)

dec :: BFProg
dec = withCurrent (chr . pred . ord)

putC :: BFProg
putC = do v <- get
          liftIO $ putChar . current $ v

parse :: String -> BFProg
parse s = sequence_ $ map cmd s

cmd :: Char -> BFProg
cmd '+' = inc
cmd '-' = dec
cmd '.' = putC
cmd _   = return ()

main :: IO ()
main = do (file:_) <- getArgs
          code <- readFile file
          evalStateT (parse code) (BFState [] (chr 0) [])

-- newtype Prog m  a = Prog {runProg :: State -> m (State, a)}

-- instance Monad m => Monad (Prog m) where
--    return a   = Prog (\state -> return (state, a))
--    prog >>= f = Prog (\state -> do (state', a) <- runProg prog state
--                                    (state'', a') <- runProg (f a) state'
--                                    return (state'', a'))

-- type BFProg = Prog IO ()

-- inc :: BFProg
-- inc = Prog (\s -> do s 

-- read :: IO 
-- read 


-- eval


-- main :: IO ()
-- main = do execState [] 
--           return ()
