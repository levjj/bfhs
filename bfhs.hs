import Control.Monad.IO.Class
import Control.Monad.Trans.State

data BFState = BFState {pointer :: Int} deriving (Show)

type BFProg = StateT BFState IO ()

inc :: BFProg
inc = modify $ \st -> BFState (pointer st + 1)

dec :: BFProg
dec = modify $ \st -> BFState (pointer st - 1)

prin :: BFProg
prin = do v <- get
          liftIO $ print . pointer $ v

miniprog :: BFProg
miniprog = do inc
              inc
              inc
              prin
              dec
              prin
              dec
              prin

main :: IO ()
main = evalStateT miniprog (BFState 0)

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
