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
    k <- readInt
    poems <- replicateM n (replicateM 4 readString)
    return (n, k, poems)
  where
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace
    readInteger = state $ fromJust . BS.readInteger . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace
    readLine = state $ BS.span (not . isEoln) . BS.dropWhile isEoln
    isEoln ch = ch == '\r' || ch == '\n'

main = print =<< solve . evalState parseInput <$> BS.getContents

isVowel a = a `elem` "aeiou"

checkRhyme :: Int -> ByteString -> ByteString -> Bool
checkRhyme k a b
    | length indexA < k = False
    | length indexB < k = False
    | otherwise         = suffixA == suffixB
  where
    indexA = reverse $ BS.findIndices isVowel a
    indexB = reverse $ BS.findIndices isVowel b
    
    suffixA = BS.drop (indexA !! (k - 1)) a
    suffixB = BS.drop (indexB !! (k - 1)) b

data Outcome = AABB
             | ABAB
             | ABBA
             | AAAA
             | NoneAbove
  deriving Eq

mergeOutcome NoneAbove _ = NoneAbove
mergeOutcome _ NoneAbove = NoneAbove
mergeOutcome AAAA x = x
mergeOutcome x AAAA = x
mergeOutcome a b | a == b    = a
                 | otherwise = NoneAbove

instance Show Outcome where
    show AABB = "aabb"
    show ABAB = "abab"
    show ABBA = "abba"
    show AAAA = "aaaa"
    show NoneAbove = "NO"

checkQuatrain k [a, b, c, d] | check a b && check a c && check a d = AAAA
                             | check a b && check c d              = AABB
                             | check a c && check b d              = ABAB
                             | check a d && check b c              = ABBA
                             | otherwise                           = NoneAbove
  where
    check = checkRhyme k

solve (n, k, poems) = foldl mergeOutcome AAAA (map (checkQuatrain k) poems)

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
