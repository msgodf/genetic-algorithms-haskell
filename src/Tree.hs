{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Tree
  ( trees
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


class (Operator k a) where
  operate :: k -> a -> a -> a

-- I don't love these typeclasses, but they allow me to make things generic.
class (Fit a) => (Example a) where
  examples :: [(a,a)]

class Substitutable a where
  substitute :: a -> a -> a

class (Fit a) where
  difference :: a -> a -> a -- used to calculate the difference between the input and output of a function
  toDouble :: a -> Double


instance (Substitutable b, Ord b, Eq a, Example b, Random b, Random a, Operator a b) => Ord (Tree a b) where
    a `compare` b = fitness a `compare` fitness b
    (<=) a b = fitness a <= fitness b

instance (Substitutable b, Ord b, Random b, Eq a, Example b, Random a, Operator a b) => Genetic (Tree a) b where
    fitness x = (programFitness x) - programLengthFitnessWeighting * fromIntegral (treeSize x)
    mutate = subtreeMutation mutationProbability
    crossover = crossoverNodes

instance Bifunctor Tree where
  bimap f g (Branch o l r) = Branch (f o) (bimap f g l) (bimap f g r)
  bimap f g (Leaf x) = Leaf (g x)

instance Bifoldable Tree where
  bifoldr _ g z (Leaf v) = g v z
  bifoldr f g z (Branch o l r) = f o (bifoldr f g (bifoldr f g z r) l)

instance Random (ArithmeticFunction) where
  random g = let (x, g2) = randomR (0, 3 :: Int) g in ([Add, Subtract, Multiply, Divide] !! x, g2)
  randomR _ g = random g

instance (Eq a, Fractional a) => Operator ArithmeticFunction (Terminal a) where
  operate Add (Constant x) (Constant y) = Constant (x + y)
  operate Subtract (Constant x) (Constant y) = (Constant (x - y))
  operate Multiply (Constant x) (Constant y) = (Constant (x * y))
  operate Divide (Constant x) (Constant y) = if x == 0 || y == 0 then (Constant largeFractional) else (Constant (x / y))

-- We should never be comparing variables (because we don't know their value)
instance (Ord a) => Ord (Terminal a) where
  (Constant x) `compare` (Constant y) = x `compare` y
  (<=) (Constant x) (Constant y) = x <= y

instance Substitutable (Terminal a) where
  substitute y X = y
  substitute y x = x

instance Example (Terminal Double) where
  examples = [ (Constant (0.0 :: Double), Constant (0.0 :: Double))
             , (Constant (-2.0), Constant (-1.0))
             , (Constant 2.0, Constant 1.0)]

instance Fit (Terminal Double) where
  difference (Constant x) (Constant y) = Constant (abs (x - y))
  toDouble (Constant x) = x

instance (Random b, Random a, Operator a b) => Random (Tree a b) where
  random = randomTree
  randomR _ = random

instance (Random a) => Random (Terminal a) where
  random g = let (x, g2) = random g in
    case x of
      True -> let (value, g3) = random g2 in (Constant value, g3)
      False -> (X, g2)
  randomR _ = random

programFitness :: (Operator a b, Substitutable b, Bifoldable p, Bifunctor p, Example b) => p a b -> Double
programFitness x = - (sum $ map (\(input,output) -> toDouble (difference (head $ evaluate x input) output)) examples)

evaluate :: (Example b, Operator a b, Substitutable b, Bifoldable p, Bifunctor p) => p a b -> b -> [b]
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

flipBranches :: (Tree a b) -> (Tree a b)
flipBranches (Branch o a b) = Branch o b a
flipBranches (Leaf a) = Leaf a

treeDepth :: (Tree a b) -> Int
treeDepth t = f t 0 where
  f = (\t d -> case t of (Branch o a b) -> if m > n
                                           then m
                                           else n where m = f a (d + 1)
                                                        n = f b (d + 1)
                         (Leaf a) -> d)

labelTree :: (Tree a b) -> Set Int
labelTree t = f t 0 empty where
  f = (\t n xs -> case t of (Leaf a) -> (singleton n) `union` xs
                            (Branch o a b) -> (singleton n) `union`
                                              (f a (2*n + 1) xs) `union`
                                              (f b (2*n + 2) xs))

intersectionOfTreeLabels :: (Tree a b) -> (Tree a b) -> Set Int
intersectionOfTreeLabels t1 t2 = (labelTree t2) `intersection` (labelTree t1)

swapNodes :: ((Tree a b), (Tree a b)) -> Int -> ((Tree a b), (Tree a b))
swapNodes ts n = f ts n 0 where
  f = (\ts n m -> case ts of
          (Branch o1 a b, Branch o2 c d) -> let (a2, c2) = f (a, c) (2*n + 1) m
                                                (b2, d2) = f (b, d) (2*n + 2) m in
                                              if n == m
                                              then (Branch o2 c2 d2, Branch o1 a2 b2)
                                              else (Branch o1 a2 b2, Branch o2 c2 d2)
          (a, b) -> if n == m then (b, a) else (a, b))

substituteNthNode :: (Tree a b) -> (Tree a b) -> Int -> (Tree a b)
substituteNthNode t1 t2 n = f t1 t2 n 0 where
  f = (\t1 t2 n m -> case t1 of
          (Leaf a) -> if n == m then t2 else (Leaf a)
          (Branch o a b) -> if n == m then t2
                            else
                              if m > n
                              then Branch o a b
                              else Branch o (f a t2 n (2*m + 1)) (f b t2 n (2*m + 2)))

subtreeMutation :: (RandomGen g, Random b, Random a, Operator a b) => Double -> (Tree a b) -> g -> ((Tree a b), g)
subtreeMutation p t g  = let (r, g2) = randomR (0, 1 :: (Double)) g in
                          if r > p
                          then (t, g2)
                          else let labels = labelTree t
                                   (p, g3) = randomR (0, length labels - 1) g2
                                   n = elemAt p labels
                                   (t2, g4) = randomTree g3 in
                            (substituteNthNode t t2 n, g4)

crossoverNodes :: (RandomGen g) => ((Tree a b), (Tree a b)) -> g -> (((Tree a b), (Tree a b)), g)
crossoverNodes (a, b) g = let intersectionSet = intersectionOfTreeLabels a b
                              (crossoverPoint, g2) = randomR (0, ((length intersectionSet) - 1)) g in
                           (swapNodes (a,b) (elemAt crossoverPoint intersectionSet), g2)

containsVariables :: (Tree a (Terminal b)) -> Bool
containsVariables (Branch o a b) = containsVariables a || containsVariables b
containsVariables (Leaf X) = True
containsVariables x = False

treeSize :: (Tree a b) -> Int
treeSize x = length $ labelTree x

waste g = (\(_,g) -> g) $ (random g :: (Int,StdGen))
