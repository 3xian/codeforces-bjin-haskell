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

import Debug.Trace

parseInput = do 
    n <- readInt
    m <- readInt
    k <- readInt
    return (n, m, k)
  where
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace
    readInteger = state $ fromJust . BS.readInteger . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace
    readLine = state $ BS.span (not . isEoln) . BS.dropWhile isEoln
    isEoln ch = ch == '\r' || ch == '\n'

main = print =<< solve . evalState parseInput <$> BS.getContents

modulus :: Integral a => a
modulus = 10^9 + 7

newtype ModP = ModP { unModP :: Int64 } deriving Eq

instance Show ModP where
    show (ModP a) = show a

instance Num ModP where
    ModP a + ModP b = ModP ((a + b) `mod` modulus)
    ModP a - ModP b = ModP ((a - b) `mod` modulus)
    ModP a * ModP b = ModP ((a * b) `mod` modulus)
    fromInteger a = ModP (fromInteger a `mod` modulus)
    abs = undefined
    signum = undefined

solve (n, m, k) = solve1D (n, k) * solve1D (m, k)

solve1D :: (Int, Int) -> ModP
solve1D (n, k) = choose (n - 1, 2 * k)

combineLine xs = zipWith (+) ([0] ++ xs) (xs ++ [0])
combineLines = iterate combineLine [1]

choose :: (Int, Int) -> ModP
choose (x, y) 
    | y < 0 || y > x = 0
    | otherwise      = ModP (cache ! (x, y))
  where
    bnds = ((0, 0), (1024, 1024))
    cache = array bnds [ ((i, j), unModP cij)
                       | (i, ri) <- zip [0..1024] combineLines
                       , (j, cij) <- zip [0..] ri
                       ] :: UArray (Int, Int) Int64
                             

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
