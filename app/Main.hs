import Data.Maybe
import Options.Applicative

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

data MyEnv = MyEnv {
    intValue :: Int
  , strValue :: String
} deriving (Show)

myStatefulComp :: Int -> State MyEnv Int
myStatefulComp x = do
  env <- get
  set $ env { intValue = (intValue env) + x }
  return $ x + 10

myOtherStatefulComp :: Int -> State MyEnv Int
myOtherStatefulComp x = do
  env <- get
  set $ env { intValue = (intValue env) + x }
  return $ x + 20

combined :: State MyEnv Int
combined = do
  myStatefulComp 5 >>= \int1 ->
    myStatefulComp 10 >>= \int2 ->
      return $ int1 + int2

doTheThings :: MyEnv -> (Int, MyEnv)
doTheThings env = runState combined env

-- > doTheThings (MyEnv { intValue = 0, strValue = "" })
--   (35,MyEnv { intValue = 15, strValue = "" })

instance Functor (State s) where
  fmap = undefined

instance Applicative (State s) where
  pure = undefined
  (<*>) = undefined

instance Monad (State s) where
  return x = State $ \s -> (x, s)
  initComp >>= f = State $ \initEnv ->
    let (value, newEnv) = runState initComp initEnv
        newComp = f value
        in runState newComp newEnv
                                     
runState :: State s a -> s -> (a, s)
runState (State f) s = f s

get :: State s s
get = State $ \s -> (s, s)

set :: s -> State s ()
set newState = State $ \_ -> ((), newState)

-- execState :: State s a -> s -> s
-- evalState :: State s a -> s -> a
-- runState :: State s a -> s -> (a, s)

main = execParser opts >>= printFazzbozz

printFazzbozz :: CmdOptions Integer -> IO ()
printFazzbozz (CmdOptions n matchSpecs) =
  mapM_ putStrLn $ scanM sfazzbozz states [1..n]
    where
      states = map (fmap makeState) matchSpecs
