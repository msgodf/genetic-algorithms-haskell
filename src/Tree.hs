{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Tree
  ( programs
  , Program(..)
  , Tree(..)
  , Terminal(..)
  , ArithmeticFunction(..)
  , Operator(..)
  ) where

import Genetic (Genetic(..))
import System.Random
import Data.Bifunctor
import Data.Bifoldable
import Data.Ord (Ord(..))
import Data.Set ( Set
                , elemAt
                , intersection
                , union
                , singleton
                , empty)

maximumTreeDepth = 8
mutationProbability = 0.8
programLengthFitnessWeighting = 0.9

-- This is a large Fractional, to return when a division by zero happens, preventing such programs
-- winning. Some exceptional behaviour might be better, but this is a fix for now.
largeFractional :: (Fractional a) => a
largeFractional = 1e20

data ArithmeticFunction = Add | Subtract | Multiply | Divide deriving (Show, Eq)

data Terminal a = Constant a | X deriving (Show, Eq)

data Tree a b = Leaf b | Branch a (Tree a b) (Tree a b) deriving (Show, Eq)

data Program a b = Program {
    ast :: Tree a b
  , inputsAndOutputs :: [(b, b)]
} deriving (Show, Eq)

class Operator k a where
  operate :: k -> a -> a -> a

class Substitutable a where
  substitute :: a -> a -> a

class Fit a where
  difference :: a -> a -> a -- used to calculate the difference between the input and output of a function
  toDouble :: a -> Double

instance (Substitutable b, Ord b, Eq a, Random b, Random a, Operator a b, Fit b) => Ord (Program a b) where
    a `compare` b = fitness a `compare` fitness b
    (<=) a b = fitness a <= fitness b

instance (Substitutable b, Ord b, Random b, Eq a, Random a, Operator a b, Fit b) => Genetic (Program a) b where
    fitness = programFitness
    mutate = mutateProgram
    crossover = crossoverProgramASTs

instance Bifunctor Tree where
  bimap f g (Branch o l r) = Branch (f o) (bimap f g l) (bimap f g r)
  bimap f g (Leaf x) = Leaf (g x)

instance Bifoldable Tree where
  bifoldr _ g z (Leaf v) = g v z
  bifoldr f g z (Branch o l r) = f o (bifoldr f g (bifoldr f g z r) l)

instance Random ArithmeticFunction where
  random g = let (x, g2) = randomR (0, 3 :: Int) g in ([Add, Subtract, Multiply, Divide] !! x, g2)
  randomR _ g = random g

instance (Eq a, Fractional a) => Operator ArithmeticFunction (Terminal a) where
  operate Add (Constant x) (Constant y) = Constant (x + y)
  operate Subtract (Constant x) (Constant y) = (Constant (x - y))
  operate Multiply (Constant x) (Constant y) = (Constant (x * y))
  operate Divide (Constant x) (Constant y) = if x == 0 || y == 0 then (Constant largeFractional) else (Constant (x / y))

-- We should never be comparing variables (because we don't know their value)
instance Ord a => Ord (Terminal a) where
  (Constant x) `compare` (Constant y) = x `compare` y
  (<=) (Constant x) (Constant y) = x <= y

instance Substitutable (Terminal a) where
  substitute y X = y
  substitute y x = x

instance Fit (Terminal Double) where
  difference (Constant x) (Constant y) = Constant (abs (x - y))
  toDouble (Constant x) = x

instance (Random b, Random a, Operator a b) => Random (Tree a b) where
  random = randomTree
  randomR _ = random

instance Random a => Random (Terminal a) where
  random g = let (x, g2) = random g in
    case x of
      True -> let (value, g3) = random g2 in (Constant value, g3)
      False -> (X, g2)
  randomR _ = random

programFitness :: (Operator a b, Substitutable b, Fit b) => Program a b -> Double
programFitness (Program { ast = x, inputsAndOutputs = y }) = (treeFitness x y) - programLengthFitnessWeighting * fromIntegral (treeSize x)

treeFitness :: (Operator a b, Substitutable b, Bifoldable p, Fit b,  Bifunctor p) => p a b -> [(b, b)] -> Double
treeFitness x examples = - (sum $ map (\(input, output) -> toDouble (difference (head $ evaluate x input) output)) examples)

evaluate :: (Operator a b, Substitutable b, Bifoldable p, Bifunctor p) => p a b -> b -> [b]
evaluate x y = reduceTree $ bimap id (substitute y) x

reduceTree :: (Operator a b, Bifoldable p) => p a b ->  [b]
reduceTree = bifoldr f (:) [] where f = (\o xs -> case xs of (x:y:_) -> [operate o x y]
                                                             (x:_) -> [x])

randomTree :: (RandomGen g, Random b, Random a) => g -> (Tree a b, g)
randomTree g = (\(x, _, z) -> (x, z)) $ f 0 g where
  f = (\d g -> if d >= maximumTreeDepth
               then
                 let (value, g2) = random g in (Leaf value, d + 1, g2)
               else
                 let (x, g2) = random g in
                   case x of
                     True -> let (value, g3) = random g2 in
                               (Leaf value, d, g3)
                     False -> let (operation, g3) = random g2
                                  (left, d2, g4) = f (d + 1) g3
                                  (right, d3, g5) = f (d + 1) g4 in
                            ((Branch operation left right), d3, g5))

prependAndThread f (xs, g) = (\(x, g) -> (x:xs, g)) $ f g

trees :: (RandomGen g, Random b, Random a) => Int -> g -> ([(Tree a b)], g)
trees n g = iterate (prependAndThread randomTree) ([], g) !! n

programs :: (RandomGen g, Random b, Random a) => Int -> g -> [(b,b)] -> ([Program a b], g)
programs n g examples = let (xs, g2) = trees n g in ((fmap (\t -> Program t examples) xs), g2)

flipBranches :: Tree a b -> Tree a b
flipBranches (Branch o a b) = Branch o b a
flipBranches (Leaf a) = Leaf a

treeDepth :: Tree a b -> Int
treeDepth t = f t 0 where
  f = (\t d -> case t of (Branch o a b) -> if m > n
                                           then m
                                           else n where m = f a (d + 1)
                                                        n = f b (d + 1)
                         (Leaf a) -> d)

floorlog2 :: Integer -> Integer
floorlog2 0 = error "floor(log2(x)) called with x = 0"
floorlog2 x = toInteger(floor(logBase 2.0 ((fromInteger x) :: Double)))

branchLabels :: a -> [[Integer]] -> [[Integer]]
branchLabels _ (l:r:rest) = [0:leftSubtree ++ rightSubtree] ++ rest
  where leftSubtree = fmap (\x -> x + 2^(floorlog2 (x + 1))) l
        rightSubtree = fmap (\x -> x + 2^(1 + floorlog2 (x + 1))) r

labelTree :: (Bifoldable p) => p a b ->  Set Integer
labelTree = fromList . head . bifoldr branchLabels (\_ xs -> [0]:xs) []

intersectionOfTreeLabels :: Tree a b -> Tree a b -> Set Integer
intersectionOfTreeLabels t1 t2 = (labelTree t2) `intersection` (labelTree t1)

swapNodes :: ((Tree a b), (Tree a b)) -> Integer -> ((Tree a b), (Tree a b))
swapNodes ts n = f ts n 0 where
  f = (\ts n m -> case ts of
          (Branch o1 a b, Branch o2 c d) -> let (a2, c2) = f (a, c) (2*n + 1) m
                                                (b2, d2) = f (b, d) (2*n + 2) m in
                                              if n == m
                                              then (Branch o2 c2 d2, Branch o1 a2 b2)
                                              else (Branch o1 a2 b2, Branch o2 c2 d2)
          (a, b) -> if n == m then (b, a) else (a, b))

substituteNthNode :: Tree a b -> Tree a b -> Integer -> Tree a b
substituteNthNode t1 t2 n = f t1 t2 n 0 where
  f = (\t1 t2 n m -> case t1 of
          (Leaf a) -> if n == m then t2 else (Leaf a)
          (Branch o a b) -> if n == m then t2
                            else
                              if m > n
                              then Branch o a b
                              else Branch o (f a t2 n (2*m + 1)) (f b t2 n (2*m + 2)))

mutateProgram :: (RandomGen g, Random b, Random a, Operator a b) => Program a b -> g -> (Program a b, g)
mutateProgram x g = let (ast1, g2) = subtreeMutation mutationProbability (ast x) g in (x { ast = ast1 }, g2)

subtreeMutation :: (RandomGen g, Random b, Random a, Operator a b) => Double -> (Tree a b) -> g -> ((Tree a b), g)
subtreeMutation p t g  = let (r, g2) = randomR (0, 1 :: (Double)) g in
                          if r > p
                          then (t, g2)
                          else let labels = labelTree t
                                   (p, g3) = randomR (0, length labels - 1) g2
                                   n = elemAt p labels
                                   (t2, g4) = randomTree g3 in
                            (substituteNthNode t t2 n, g4)

crossoverProgramASTs :: RandomGen g => (Program a b, Program a b) -> g -> ((Program a b, Program a b), g)                            
crossoverProgramASTs (x, y) g = let ((ast1, ast2), g2) = (crossoverNodes (ast x, ast y) g) in (( (x { ast = ast1 }), (y { ast = ast2})), g2)

crossoverNodes :: RandomGen g => ((Tree a b), (Tree a b)) -> g -> (((Tree a b), (Tree a b)), g)
crossoverNodes (a, b) g = let intersectionSet = intersectionOfTreeLabels a b
                              (crossoverPoint, g2) = randomR (0, ((length intersectionSet) - 1)) g in
                           (swapNodes (a,b) (elemAt crossoverPoint intersectionSet), g2)

containsVariables :: (Tree a (Terminal b)) -> Bool
containsVariables (Branch o a b) = containsVariables a || containsVariables b
containsVariables (Leaf X) = True
containsVariables x = False

treeSize :: Tree a b -> Int
treeSize = bifoldr f f 0 where f = (\ _ x -> x + 1)

waste g = (\(_,g) -> g) $ (random g :: (Int,StdGen))
