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
    n <- readInt
    m <- readInt
    return (n, m)
  where
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace
    readInteger = state $ fromJust . BS.readInteger . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace
    readLine = state $ BS.span (not . isEoln) . BS.dropWhile isEoln
    isEoln ch = ch == '\r' || ch == '\n'

main = print =<< solve . evalState parseInput <$> BS.getContents

inf = 10^9

solve (n, m) | n < m = solve (m, n)
solve (n, m) = n * m - go (0, 0) [((1 `shiftL` m - 1) `shiftL` m, 0)]
  where
    go (x, y) lst | y == m = go (x+1, 0) lst
    go (x, y) lst | x == n = fromMaybe inf $ lookup 0 lst
    go (x, y) lst = go (x, y+1) [(msk, value) | (msk, value) <- assocs arr, value < inf]
      where
        arr = accumArray min inf (0, 1 `shiftL` (m * 2)) updates :: UArray Int Int
        updates = updates0 ++ updates1
        updates0 = [ (msk' `shiftR` 1, value)
                   | (msk, value) <- lst
                   , let msk' = if x + 1 < n then msk .|. (1 `shiftL` (m + m)) else msk
                   , not $ msk `testBit` 0
                   ]
        updates1 = [ (msk'' `shiftR` 1, value + 1)
                   | (msk, value) <- lst
                   , let msk' = if x + 1 < n then msk .|. (1 `shiftL` (m + m)) else msk
                   , let left = [m - 1 | y > 0]
                   , let right = [m + 1 | y + 1 < m]
                   , let up = [0 | x > 0]
                   , let down = [m + m | x + 1 < n]
                   , let all = sum [1 `shiftL` bit | bit <- left ++ right ++ up ++ down ++ [m]]
                   , let msk'' = msk' .&. complement all
                   , not $ msk'' `testBit` 0
                   ]

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
