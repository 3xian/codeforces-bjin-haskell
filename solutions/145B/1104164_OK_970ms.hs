{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# OPTIONS_GHC -O2 #-}

import Data.List
import Data.Maybe
import Data.Char
import Data.Array.IArray
import Data.Array.Unboxed (UArray)
import Data.Int
import Data.Ratio
import Data.Bits
import Data.Function
import Data.Ord
--import Control.Monad.State
import Control.Monad
import Control.Applicative
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Sequence (Seq, (<|), (|>), (><), ViewL(..), ViewR(..))
import qualified Data.Sequence as Seq
import qualified Data.Foldable as F
import Data.Graph

parseInput = do 
    cnt4 <- readInt
    cnt7 <- readInt
    cnt47 <- readInt
    cnt74 <- readInt
    return (cnt4, cnt7, cnt47, cnt74)
  where
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace
    readInteger = state $ fromJust . BS.readInteger . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace
    readLine = state $ BS.span (not . isEoln) . BS.dropWhile isEoln
    isEoln ch = ch == '\r' || ch == '\n'

main = putStrLn =<< solve . evalState parseInput <$> BS.getContents

check4 cnt4 cnt7 cnt47 cnt74
    | cnt4 < 0 || cnt7 < 0    = False
    | cnt47 < 0 || cnt74 < 0  = False
    | cnt44 < 0               = False
    | cnt77 < 0               = False
    | cnt77 > 0 && cnt47 == 0 = False
    | delta < 0 || delta > 1  = False
    | otherwise               = True
  where
    cnt44 = cnt4 - cnt74
    cnt77 = cnt7 - cnt47

    delta = cnt47 - cnt74 -- must be either 0 or 1

solve (cnt4, cnt7, cnt47, cnt74)
    | first4    = go (cnt4 - 1) cnt7 cnt47 cnt74 '4' "4"
    | first7    = go (cnt7 - 1) cnt4 cnt74 cnt47 '7' "7"
    | otherwise = "-1"
  where
    first4 = check4 (cnt4 - 1) cnt7 cnt47 cnt74
    first7 = check4 (cnt7 - 1) cnt4 cnt74 cnt47

    op '4' = '7'
    op '7' = '4'

    go 0 0 0 0 x xs = reverse xs
    go cnt4 cnt7 cnt47 cnt74 x xs
        | okay4 && okay7 = if x == '4' then go4 else go7
        | okay4          = go4
        | okay7          = go7
        | otherwise      = error "internal error"
      where
        okay4 = check4 (cnt4 - 1) cnt7 cnt47 cnt74
        okay7 = check4 (cnt7 - 1) cnt4 cnt74 (cnt47 - 1)

        go4 = go (cnt4 - 1) cnt7 cnt47 cnt74 x (x:xs)
        go7 = go (cnt7 - 1) cnt4 cnt74 (cnt47 - 1) (op x) (op x:xs)

----------------------------------------------------------------------
----------------------------------------------------------------------
----------------------------------------------------------------------

class (Monad m) => MonadState s m | m -> s where
	get :: m s
	put :: s -> m ()

modify :: (MonadState s m) => (s -> s) -> m ()
modify f = do
	s <- get
	put (f s)

gets :: (MonadState s m) => (s -> a) -> m a
gets f = do
	s <- get
	return (f s)

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
	fmap f m = State $ \s -> let
		(a, s') = runState m s
		in (f a, s')

instance Applicative (State s) where
    pure = return
    (<*>) = ap

instance Monad (State s) where
	return a = State $ \s -> (a, s)
	m >>= k  = State $ \s -> let
		(a, s') = runState m s
		in runState (k a) s'

instance MonadState s (State s) where
	get   = State $ \s -> (s, s)
	put s = State $ \_ -> ((), s)

evalState :: State s a -> s -> a
evalState m s = fst (runState m s)

execState :: State s a -> s -> s
execState m s = snd (runState m s)

mapState :: ((a, s) -> (b, s)) -> State s a -> State s b
mapState f m = State $ f . runState m

withState :: (s -> s) -> State s a -> State s a
withState f m = State $ runState m . f

state = State
