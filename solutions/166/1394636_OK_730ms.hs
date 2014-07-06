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

import Debug.Trace

parseInput = do 
    n <- readInt
    a <- replicateM n ((,) <$> readInt64 <*> readInt64)
    m <- readInt
    b <- replicateM m ((,) <$> readInt64 <*> readInt64)
    return (reverse a, b)
  where
    readInt64 = fromIntegral <$> readInt :: State ByteString Int64
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace
    readInteger = state $ fromJust . BS.readInteger . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace
    readLine = state $ BS.span (not . isEoln) . BS.dropWhile isEoln
    isEoln ch = ch == '\r' || ch == '\n'

main = BS.putStr =<< solve . evalState parseInput <$> BS.getContents

det :: Num a => (a, a) -> (a, a) -> a
det (x1, y1) (x2, y2) = x1 * y2 - x2 * y1
{-# INLINE det #-}

dot :: Num a => (a, a) -> (a, a) -> a
dot (x1, y1) (x2, y2) = x1 * x2 + y1 * y2
{-# INLINE dot #-}

det3 :: Num a => (a, a) -> (a, a) -> (a, a) -> a
det3 (x0, y0) (x1, y1) (x2, y2) = det (x1 - x0, y1 - y0) (x2 - x0, y2 - y0)
{-# INLINE det3 #-}

convex :: [(Int64, Int64)] -> [(Int64, Int64)]
convex pts' = leftToRight pts []
  where
    pts = sort pts'

    leftToRight [] stack = rightToLeft (tail $ reverse pts) stack
    leftToRight (p:ps) (a:b:cs)
        | det3 b a p < 0 = leftToRight (p:ps) (b:cs)
    leftToRight (p:ps) st = leftToRight ps (p:st)

    rightToLeft [] stack = reverse $ tail stack
    rightToLeft (p:ps) (a:b:cs)
        | det3 b a p < 0 = rightToLeft (p:ps) (b:cs)
    rightToLeft (p:ps) st = rightToLeft ps (p:st)

area :: [(Int64, Int64)] -> Int64
area (x:xs) = foldl' (+) 0 $ zipWith det (x:xs) (xs++[x])

solve (a, b)
    | area a == area c && length a == length c = "YES"
    | otherwise                                = "NO"
  where
    c = convex (a ++ b)

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
