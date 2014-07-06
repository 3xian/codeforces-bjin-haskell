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
    s <- readString
    return s
  where
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace
    readInteger = state $ fromJust . BS.readInteger . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace
    readLine = state $ BS.span (not . isEoln) . BS.dropWhile isEoln
    isEoln ch = ch == '\r' || ch == '\n'

main = BS.putStr =<< solve . evalState parseInput <$> BS.getContents

makeString :: (Int -> Int) -> ByteString
makeString f = BS.concat [BS.replicate (f x) (intToDigit x) | x <- [0..9]] 

check9 :: (Int -> Int) -> (Int -> Int) -> Int -> (Int, ByteString, ByteString)
check9 f g r = (ansLen + 1, ansf, ansg)
  where
    ansLen = sum [f i `min` g (9 - i) | i <- [0..9]] + zeros
    ansf = makeString f'' `BS.append` middle `BS.append` BS.singleton (intToDigit r) `BS.append` BS.replicate zeros '0'
    ansg = makeString g'' `BS.append` BS.map (\x -> intToDigit (9 - digitToInt x)) middle `BS.append` BS.singleton (intToDigit (10 - r)) `BS.append` BS.replicate zeros '0'

    middle = makeString (\i -> f i `min` g (9 - i))

    zeros = f' 0 `min` g' 0

    f' x = max 0 (f x - g (9 - x))
    g' x = max 0 (g x - f (9 - x))
    f'' x | x == 0    = f' x - zeros
          | otherwise = f' x
    g'' x | x == 0    = g' x - zeros
          | otherwise = g' x

decrease :: Int -> (Int -> Int) -> (Int -> Int)
decrease k f x | x == k    = f x - 1
               | otherwise = f x

check10 :: (Int -> Int) -> (Int, ByteString, ByteString)
check10 f = maximum ans
  where
    ans = [ check9 (decrease i f) (decrease (10 - i) f) i
          | i <- [1..5]
          , f i > 0 && f (10 - i) > 0
          ] ++ [(f 0, BS.reverse $ makeString f, BS.reverse $ makeString f)]

solve s = case check10 (num !!) of
    (len, f, g) -> traceShow len $ BS.unlines [f, g]
  where
    num = map (`BS.count` s) ['0'..'9']

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
