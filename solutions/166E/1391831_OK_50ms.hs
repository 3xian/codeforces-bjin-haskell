{-# LANGUAGE RankNTypes, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, OverloadedStrings, ScopedTypeVariables, FlexibleContexts #-}
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
import Control.Applicative hiding ((*>),(<*))
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

parseInput = readInt
  where
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace
    readInteger = state $ fromJust . BS.readInteger . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace
    readLine = state $ BS.span (not . isEoln) . BS.dropWhile isEoln
    isEoln ch = ch == '\r' || ch == '\n'

main = print =<< solve . evalState parseInput <$> BS.getContents

solve :: Int -> Integer
solve n = solve' n `modP` (10^9 + 7)

solve' :: Num a => Int -> a
solve' = runLinearRecursive $ do
    a <- newVariable 1
    b <- newVariable 0
    c <- newVariable 0
    d <- newVariable 0
    a <:- b <+> c <+> d
    b <:- a <+> c <+> d
    c <:- a <+> b <+> d
    d <:- a <+> b <+> c
    return a
        

--{{{ simple monad-lrs
newtype Vector a = Vector { unVector :: IntMap a }

vector :: Num a => IntMap a -> Vector a
vector = Vector

newtype Vector1 a = Vector1 { unVector1 :: Int }

vector1 :: Num a => Int -> Vector1 a
vector1 = Vector1

class VectorLike v where
    toVector :: Num a => v a -> Vector a

instance VectorLike Vector where
    toVector = id

instance VectorLike Vector1 where
    toVector (Vector1 p) = Vector (IntMap.singleton p 1)

instance Functor Vector where
    fmap f = Vector . IntMap.map f . unVector

unVector' :: (Num a, VectorLike v) => v a -> IntMap a
unVector' = unVector . toVector

(<+>) :: (Num a, VectorLike v1, VectorLike v2) => v1 a -> v2 a -> Vector a
a <+> b = Vector $ IntMap.unionWith (+) (unVector' a) (unVector' b)

(<->) :: (Num a, VectorLike v1, VectorLike v2) => v1 a -> v2 a -> Vector a
a <-> b = a <+> fmap negate (toVector b)

(*>) :: (Num a, VectorLike v) => v a -> a -> Vector a
a *> b = fmap (*b) (toVector a)

(<*) :: (Num a, VectorLike v) => a -> v a -> Vector a
a <* b = fmap (a*) (toVector b)

infixl 6 <+>,<->
infixl 7 *>
infixr 7 <*

emptyVector :: Num a => Vector a
emptyVector = Vector IntMap.empty

data Matrix a = Matrix { unMatrix :: [[a]] }
              | Diagonal { unDiagonal :: [a] }
    deriving (Show, Eq)

toMatrix :: Num a => Matrix a -> Matrix a
toMatrix (Matrix a) = Matrix a
toMatrix (Diagonal a) = Matrix [replicate i 0 ++ [aii] ++ repeat 0 | (i, aii) <- zip [0..] a]

unMatrix' :: Num a => Matrix a -> [[a]]
unMatrix' = unMatrix . toMatrix

matrix :: [[a]] -> Matrix a
matrix = Matrix

diagonal :: [a] -> Matrix a
diagonal = Diagonal

instance Num a => Num (Matrix a) where
    Diagonal a + Diagonal b = diagonal (zipWith (+) a b)
    a + b = matrix (zipWith (zipWith (+)) (unMatrix' a) (unMatrix' b))

    negate (Matrix a) = matrix (map (map negate) a)
    negate (Diagonal a) = diagonal (map negate a)

    fromInteger = diagonal . repeat . fromInteger

    Matrix a * Matrix b = let tb = transpose b
                              c = [[sum (zipWith (*) ra cb) | cb <- tb] | ra <- a]
                          in
                              matrix c
    Diagonal a * Diagonal b = diagonal (zipWith (*) a b)
    Diagonal a * Matrix b = matrix (zipWith (\v row -> map (v*) row) a b)
    Matrix a * Diagonal b = matrix (map (\row -> zipWith (*) row b) a)

    abs = error "Matrix: abs undefined"
    signum = error "Matrix: abs undefined"





data LRVariable a = LRV { initialValue :: a, dependency :: Vector a }

dmap :: Num a => (Vector a -> Vector a) -> LRVariable a -> LRVariable a
dmap f (LRV val dep) = LRV val (f dep)

type LRVariables a = IntMap (LRVariable a)

data LinearRecursive a b = LR { unLR :: Int -> (b, Int, LRVariables a -> LRVariables a) }

instance Num a => Monad (LinearRecursive a) where
    return a = LR (const (a, 0, id))
    a >>= b = LR $ \v -> let (ra, nva, ma) = unLR a v
                             (rb, nvb, mb) = unLR (b ra) (v + nva)
                         in
                             (rb, nva + nvb, mb . ma)

newVariable :: Num a => a -> LinearRecursive a (Vector1 a)
newVariable val0 = LR $ \v -> (vector1 v, 1, IntMap.insert v variable)
  where
    variable = LRV { initialValue = val0, dependency = emptyVector }

newVariables :: Num a => [a] -> LinearRecursive a [Vector1 a]
newVariables vals = do
    ret <- mapM newVariable vals
    zipWithM_ (<:-) (tail ret) ret
    return ret

newConstant :: Num a => a -> LinearRecursive a (Vector a)
newConstant val = do
    ret <- newVariable val
    ret <:- ret
    return (toVector ret)

(<+-) :: (Num a, VectorLike v) => Vector1 a -> v a -> LinearRecursive a ()
(<+-) var dep = LR (const ((), 0, IntMap.adjust (dmap (<+>toVector dep)) (unVector1 var)))

(<:-) :: (Num a, VectorLike v) => Vector1 a -> v a -> LinearRecursive a ()
(<:-) var dep = LR (const ((), 0, IntMap.adjust (dmap (const (toVector dep))) (unVector1 var)))

infix 1 <:-,<+-

buildMatrix :: Num a => LRVariables a -> (Matrix a, Matrix a)
buildMatrix mapping = (Matrix trans, Matrix $ map (\x -> [x]) initValues)
  where
    initValues = map initialValue (IntMap.elems mapping)
    rawDep = map (unVector'.dependency) (IntMap.elems mapping)
    varCount = length initValues
    trans = map (\m -> [IntMap.findWithDefault 0 i m | i <- [0..varCount-1]]) rawDep

runLinearRecursive :: (Num a, Integral b, VectorLike v) => LinearRecursive a (v a) -> b -> a
runLinearRecursive monad steps = sum [head (res !! i) * ai | (i, ai) <- IntMap.assocs (unVector' target)]
  where
    (target, nv, g) = unLR monad 0 
    dep = g IntMap.empty
    (trans, init) = buildMatrix dep

    Matrix res = trans^steps * init

--}}}

--{{{ ModP
newtype Dep a b = Dep { unDep :: b }

data One = One

data D0 a = D0 a
data D1 a = D1 a

class Integral b => PositiveN p b where
    p2num :: Dep p b

instance Integral b => PositiveN One b where
    p2num = Dep 1

instance PositiveN p b => PositiveN (D0 p) b where
    p2num = Dep (unDep (p2num :: Dep p b) * 2)

instance PositiveN p b => PositiveN (D1 p) b where
    p2num = Dep (unDep (p2num :: Dep p b) * 2 + 1)

newtype ModP p b = ModP { unModP :: b } deriving Eq

instance (Show b, PositiveN p b) => Show (ModP p b) where
    show (ModP r) = show r ++ "+" ++ show (unDep (p2num :: Dep p b)) ++ "Z"

instance (Show b, PositiveN p b) => Num (ModP p b) where
    ModP a + ModP b = ModP ((a + b) `mod` unDep (p2num :: Dep p b))
    ModP a - ModP b = ModP ((a - b) `mod` unDep (p2num :: Dep p b))
    ModP a * ModP b = ModP ((a * b) `mod` unDep (p2num :: Dep p b))
    fromInteger x = ModP (fromInteger x `mod` unDep (p2num :: Dep p b))
    abs = undefined
    signum = undefined

extgcd :: Integral a => a -> a -> (a, a, a)
extgcd a b | a < 0 = let (g, x, y) = extgcd (-a) b in (g, -x, y)
extgcd a b | b < 0 = let (g, x, y) = extgcd a (-b) in (g, x, -y)
extgcd a 0 = (a, 1, 0)
extgcd a b = (g, x, y - adivb * x)
  where
    (adivb, amodb) = a `divMod` b
    (g, y, x) = extgcd b amodb

instance (Show b, PositiveN p b) => Fractional (ModP p b) where
    recip (ModP a) | g /= 1    = error "ModP: division invalid"
                   | otherwise = ModP (x `mod` n)
      where
        n = unDep (p2num :: Dep p b)
        (g, x, _) = extgcd a n
    fromRational a = fromInteger (numerator a) / fromInteger (denominator a)
    

num2p' :: (Integral b, Integral i) => i -> (forall p. PositiveN p b => p -> b) -> (forall p. PositiveN p b => p -> b)
num2p' n _ | n <= 0 = error "num2p: internal error"
num2p' 1 f = f
num2p' n f | even n    = num2p' (n `div` 2) (f . D0)
           | otherwise = num2p' (n `div` 2) (f . D1)

num2p :: (Integral b, Integral i) => i -> (forall p. PositiveN p b => p -> b) -> b
num2p n f = (num2p' n f) One

modP :: (Show b, Integral b) => (forall a. Fractional a => a) -> b -> b
modP val n 
    | n <= 0    = error "modP: modulus must be positive"
    | otherwise = num2p n go
  where
    go :: forall p b. (Show b, PositiveN p b) => p -> b
    go _ = unModP (val :: ModP p b)
--}}}

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
