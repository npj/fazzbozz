import Data.Maybe
import Options.Applicative
import Text.Read

import Fazzbozz
import Fazzbozz.CmdOptions 

-- class Monad m where
--   return :: a -> m a
--   (>>=) :: m a -> (a -> m b) -> m b
-- 
-- instance Monad Maybe where
--   return x = Just x
--   Nothing >>= _ = Nothing
--   (Just x) >>= f = f x

addStuff :: Maybe Int
addStuff = return 5 >>= \x ->
   return (x + 5) >>= \y ->
    return (y + 10) >>= \_ ->
      Nothing >>= \z ->
        return (z + 15)

addStuff' :: Maybe Int
addStuff' = do
  x <- return 5
  y <- return (x + 5)
  _ <- return (y + 10)
  z <- Nothing
  return (z + 15)

data State s a = State (s -> (a, s))

instance Monad (State s) where
  return x = State $ \s -> (x, s)
  (State sf) >>= f = State $ \s -> let (a, s') = sf s
                                       (State sg) = f a
                                       in sg s'

-- execState :: State s a -> s -> a
-- evalState :: State s a -> s -> s
-- runState :: State s a -> s -> (a, s)

main = execParser opts >>= printFazzbozz

printFazzbozz :: CmdOptions Integer -> IO ()
printFazzbozz (CmdOptions n matchSpecs) =
  mapM_ putStrLn $ scanM sfazzbozz states [1..n]
    where
      states = map (fmap makeState) matchSpecs
