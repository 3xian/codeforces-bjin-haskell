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
    n <- readString
    return $ BS.unpack n
  where
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace
    readInteger = state $ fromJust . BS.readInteger . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace
    readLine = state $ BS.span (not . isEoln) . BS.dropWhile isEoln
    isEoln ch = ch == '\r' || ch == '\n'

main = putStrLn =<< solve . evalState parseInput <$> BS.getContents

check :: String -> Bool
check n = length (filter (=='4') n) * 2 == len && length (filter (=='7') n) * 2 == len
  where
    len = length n

solve :: String -> String
solve n
    | check n   = n
    | null lst  = replicate (len'+1) '4' ++ replicate (len'+1) '7'
    | otherwise = head lst
  where
    len = length n
    len' = len `div` 2

    count target = listArray (0, len) $ scanl (\num ch -> if ch == target then num + 1 else num) 0 n :: UArray Int Int
    count4 = count '4'
    count7 = count '7'

    lst = [ take pos n ++ [ch'] ++ replicate (len' - c4') '4' ++ replicate (len' - c7') '7'
          | even len
          , (pos,ch) <- zip [len-1,len-2..0] (reverse n)
          , let c4 = count4 ! pos
          , let c7 = count7 ! pos
          , c4 + c7 == pos
          , ch' <- [succ ch..'9']
          , ch' == '4' || ch' == '7'
          , let (c4', c7') = if ch' == '4' then (c4+1, c7) else (c4, c7+1)
          , abs (c4' - c7') <= len - 1 - pos
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
