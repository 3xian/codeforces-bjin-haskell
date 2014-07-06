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
import Control.Monad.ST
import Data.Array.ST

parseInput = do 
    n <- readInt
    m <- readInt
    grid <- replicateM n (BS.unpack <$> readString)
    return (n, m, grid)
  where
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace
    readInteger = state $ fromJust . BS.readInteger . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace
    readLine = state $ BS.span (not . isEoln) . BS.dropWhile isEoln
    isEoln ch = ch == '\r' || ch == '\n'

main = putStr =<< solve . evalState parseInput <$> BS.getContents

solve (n, m, grid') = show mans ++ " " ++ show (length $ filter (==mans) answer)
  where
    bnds = ((1,1),(n,m))
    grid = listArray bnds [ele | row <- grid', ele <- row] :: UArray (Int,Int) Char

    answer = [walking idx grid | idx <- range bnds, let ch = grid ! idx, ch /= '.']
    
    mans = maximum answer

getD 'U' = (-1, 0)
getD 'D' = (1, 0)
getD 'R' = (0, 1)
getD 'L' = (0, -1)

walking :: (Int,Int) -> UArray (Int,Int) Char -> Int
walking (sx, sy) grid = runST $ do
    arr <- thaw grid :: ST s (STUArray s (Int,Int) Char)
    go arr (sx, sy) (0, 0) 0
  where
    bnds = bounds grid

    go arr (x, y) (dx, dy) steps | inRange bnds (x, y) = do
        ch <- readArray arr (x, y) 
        when (ch /= '.') $ writeArray arr (x, y) '.'
        let (dx', dy') = if ch == '.' then (dx, dy) else getD ch
        let (x', y') = (x + dx', y + dy')
        let steps' = if ch == '.' then steps else steps + 1
        go arr (x', y') (dx', dy') steps'
    go arr (x, y) (dx, dy) steps = return steps

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
