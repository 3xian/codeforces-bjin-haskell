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
    grid <- replicateM n (BS.unpack <$> readString)
    return (n, m, grid)
  where
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace
    readInteger = state $ fromJust . BS.readInteger . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace
    readLine = state $ BS.span (not . isEoln) . BS.dropWhile isEoln
    isEoln ch = ch == '\r' || ch == '\n'

main = BS.putStr =<< solve . evalState parseInput <$> BS.getContents

solve :: (Int, Int, [String]) -> ByteString
solve (n, m, grid')
    | not checkAnswer = BS.pack "-1"
    | otherwise       = BS.unlines $ map BS.pack answer
  where
    bnds = ((1,1), (n,m))
    paintedGrid = transpose . paint getCh . transpose . paint (flip getCh) $ grid'
    grid = listArray bnds [ele | row <- paintedGrid, ele <- row] :: UArray (Int,Int) Char
    directions = [(-1,0),(1,0),(0,1),(0,-1)]

    avail = filter ((/='#').(grid!)) $ range bnds

    getNear (x,y) = [ np
                    | (dx, dy) <- directions
                    , let np = (x+dx, y+dy)
                    , inRange bnds np
                    , grid ! np /= '#'
                    ]

    checkAnswer = and $ map (not . null . getNear) avail

    ansGrid :: UArray (Int,Int) Char
    ansGrid = grid // (map (\x -> (x, (grid!).head.filter (isDigit.(grid!)).getNear $ x)) $ filter (not.isDigit.(grid!)) avail)
    answer = [[ansGrid ! (x, y) | y <- [1..m]] | x <- [1..n]]

paint :: (Int -> Int -> Char) -> [String] -> [String]
paint f grid = [ if length row <= 1 then row else paintedRow
               | (i, row) <- zip [0..] grid
               , let mask = map (=='.') row
               , let couldPaint = zipWith (&&) mask (tail mask)
               , let couldPaint2 = scanl1 (\x y -> not x && y) couldPaint ++ [False]
               , let paintedRow1 = [if bool then f i j else ch | (j, ch, bool) <- zip3 [0..] row couldPaint2]
               , let paintedRow = head paintedRow1 : [if bool then pch else nch | (nch, pch, bool) <- zip3 (tail paintedRow1) paintedRow1 couldPaint2]
               ]

getCh :: Int -> Int -> Char
getCh x y = chr ((x+y*3) `mod` 10 + ord '0')
    
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
