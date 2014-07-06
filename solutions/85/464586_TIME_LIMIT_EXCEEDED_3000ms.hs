{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
--{-# OPTIONS_GHC -O2 #-}

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
import Text.Printf
import Debug.Trace

parseInput = do 
    n <- readInt
    tree <- replicateM n $ (,) <$> readInt <*> readInt
    k <- readInt
    query <- replicateM k readInt
    return (n, tree, query)
  where
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace
    readInteger = state $ fromJust . BS.readInteger . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace
    readLine = state $ BS.span (not . isEoln) . BS.dropWhile isEoln
    isEoln ch = ch == '\r' || ch == '\n'

main = BS.putStr =<< solve . evalState parseInput <$> BS.getContents

solve :: (Int, [(Int,Int)], [Int]) -> ByteString
solve (n, tree, query) = BS.unlines $ map (BS.pack . printf "%.12f" . answer) query
  where
    bnds = (1, n)
    edges = [ (fa, id) | id <- range bnds, let fa = father ! id, fa /= -1]

    label = listArray bnds $ map snd tree :: UArray Int Int
    father = listArray bnds $ map fst tree :: UArray Int Int

    children = accumArray (flip (:)) [] bnds edges :: Array Int [Int]

    sibling = array bnds [(id, head $ delete id $ children!fa) | (id, fa) <- assocs father, fa /= -1] :: UArray Int Int

    getInfo :: Int -> (Int, Int)
    getInfo = (cache !)
      where
        cache = listArray bnds $ map go $ range bnds :: Array Int (Int, Int)

        go p
            | null chds = (here, here)
            | otherwise = (minv, maxv)
          where
            chds = children ! p
            here = label ! p
            chdsRes = map getInfo chds
            minv = here `min` minimum (map fst chdsRes)
            maxv = here `max` maximum (map snd chdsRes)

    getRes :: Int -> (Int, Int64)
    getRes = (cache !)
      where
        cache = listArray bnds $ map go $ range bnds :: Array Int (Int, Int64)

        go p
            | fa == -1  = (0, 0)
            | otherwise = (a + 1, b + fromIntegral b')
          where
            fa = father ! p
            sib = sibling ! p

            (a, b) = getRes fa
            b' = if label ! sib < label ! p then snd (getInfo sib) else fst (getInfo sib)

    labelDep = IntMap.fromList [(val, fst $ getRes id) | (id, val) <- assocs label]
    labelRev = IntMap.fromList [(val, id) | (id, val) <- assocs label]

    getInsertPosition :: Int -> Int
    getInsertPosition p = (labelRev IntMap.!) . fst $ maximumBy (compare `on` snd) lst
      where
        lst = [treeLo | not (IntMap.null lo)] ++ [treeHi | not (IntMap.null hi)] 

        (lo, hi) = IntMap.split p labelDep
        treeLo = IntMap.findMax lo
        treeHi = IntMap.findMin hi


    answer :: Int -> Double
    answer p = fromIntegral b / fromIntegral a
      where
        p' = getInsertPosition p
        (a, b) = getRes p'


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
