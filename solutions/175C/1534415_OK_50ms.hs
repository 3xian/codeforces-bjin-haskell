{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, OverloadedStrings #-}
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
    kc <- replicateM n ((,) <$> readInt64 <*> readInt64)
    t <- readInt
    p <- replicateM t readInt64
    return (kc, p)
  where
    readInt64 = fromIntegral <$> readInteger :: State ByteString Int64
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace
    readInteger = state $ fromJust . BS.readInteger . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace
    readLine = state $ BS.span (not . isEoln) . BS.dropWhile isEoln
    isEoln ch = ch == '\r' || ch == '\n'

main = print =<< solve . evalState parseInput <$> BS.getContents

data Chunk a = Chunk Int64 a deriving (Eq, Show)

zipWithChunk :: (a -> b -> c) -> [Chunk a] -> [Chunk b] -> [Chunk c]
zipWithChunk _ [] _ = []
zipWithChunk _ _ [] = []
zipWithChunk f (Chunk 0 _:xs) ys = zipWithChunk f xs ys
zipWithChunk f xs (Chunk 0 _:ys) = zipWithChunk f xs ys
zipWithChunk f (Chunk a x:xs) (Chunk b y:ys) = Chunk c (f x y) :
    zipWithChunk f (Chunk (a - c) x:xs) (Chunk (b - c) y:ys)
  where
    c = min a b

solve (kc', p) = sum [a * b | Chunk a b <- lst]
  where
    kc = sortBy (comparing snd) kc'

    kcs = [Chunk k c | (k, c) <- kc]
    ps = zipWith Chunk (zipWith (-) (p++[10^12]) (0:p)) [1..]

    lst = zipWithChunk (*) kcs ps :: [Chunk Int64]

-- {{{ A minimal State Monad
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
-- }}}
