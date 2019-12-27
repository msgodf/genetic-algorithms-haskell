module Tree
  ( trees
  , evaluate
  , substitute
  , Tree(..)
  , Variable(..)
  , Operation(..)
  ) where

import Genetic (Genetic(..))
import System.Random
import Data.Ord (Ord(..))
import Data.Set

data Operation = Add | Subtract | Multiply | Divide deriving (Show,Eq)

maximumTreeDepth = 10
targets = [(-3.0, 9.0), (-1.0, 1.0), (0.1, 0.01), (1.0, 1.0), (3.0, 9.0)]

instance Ord Tree where
  a `compare` b = fitness a `compare` fitness b
  (<=) a b = fitness a <= fitness b

instance Genetic Tree where
  fitness x = -(sum $ fmap (\(input, output) -> let (Leaf (Value v)) = evaluate (substitute x (Leaf (Value input))) in abs(output - v)) targets)
  mutate x g = (x, g)
  crossover (x,y) g = crossoverNodes (x,y) g

instance Random Operation where
  random g = let (x, g2) = randomR (0, 2 :: Int) g in ([Add, Subtract, Multiply, Divide] !! x, g2)
  randomR _ g = random g

data Variable a = Value a | X deriving (Show, Eq)

data Tree = Leaf (Variable Double) | Branch Operation Tree Tree deriving (Show,Eq)

instance Random Tree where
  random g = (\(x,_,y) -> (x,y)) $ randomTree 0 g
  randomR _ g = random g

randomTree ::(RandomGen g) => Int -> g -> (Tree, Int, g)
randomTree d g = if d >= maximumTreeDepth
  then
  let (value,g2) = random g in
        (Leaf (Value value), d + 1, g2)
  else
  let (x, g2) = randomR (0, 2::Int) g in
    case x of
      0 -> let (value, g3) = random g2 in
        (Leaf (Value value), d, g3)
      1 -> let (operation, g3) = random g2
               (left, d2, g4) = randomTree (d + 1) g3
               (right, d3, g5) = randomTree (d + 1) g4 in
             ((Branch operation left right), d3, g5)
      2 -> (Leaf X, d, g2)

prependAndThread f (xs, g) = (\(x, g) -> if (containsVariables x) then (x:xs, g) else (xs,g)) $ f g

trees :: (RandomGen g) => Int -> g -> ([Tree], g)
trees n g = iterate (prependAndThread (\g -> let (x,y,z) = randomTree 0 g in (x,z))) ([],g) !! n

-- Operate can only operate on values, not variables
operate :: Operation -> (Variable Double) -> (Variable Double) -> (Variable Double) 
operate Add (Value x) (Value y) = (Value (x + y))
operate Subtract (Value x) (Value y) = (Value (x - y))
operate Multiply (Value x) (Value y) = (Value (x * y))
operate Divide (Value x) (Value y) = (Value (x / y))

evaluate :: Tree -> Tree
evaluate (Leaf a) = (Leaf a)
evaluate (Branch o (Leaf a) (Leaf b)) = Leaf $ operate o a b
evaluate (Branch o a b) = evaluate $ Branch o (evaluate a) (evaluate b)

flipBranches :: Tree -> Tree
flipBranches (Branch o a b) = Branch o b a
flipBranches (Leaf a) = Leaf a

maxDepth :: Int -> Tree -> Int
maxDepth d (Branch o a b) = if m > n then m else n where m = maxDepth (d + 1) a
                                                         n = maxDepth (d + 1) b
maxDepth d (Leaf a) = d

labelTree :: Tree -> Int -> Set Int -> Set Int
labelTree (Leaf a) n xs = (singleton n) `union` xs
labelTree (Branch o a b) n xs = (singleton n) `union` (labelTree a (2*n + 1) xs) `union` (labelTree b (2*n + 2) xs)

intersectionOfTreeLabels :: Tree -> Tree -> Set Int
intersectionOfTreeLabels t1 t2 = (labelTree t2 0 empty) `intersection` (labelTree t1 0 empty)

swapNodes :: (Tree, Tree) -> Int -> Int -> (Tree, Tree)
swapNodes (Branch o1 a b, Branch o2 c d) n m = let (a2,c2) = swapNodes (a,c) (2*n + 1) m
                                                   (b2,d2) = swapNodes (b,d) (2*n + 2) m in
                                                       if n == m then (Branch o2 c2 d2, Branch o1 a2 b2)
                                                       else (Branch o1 a2 b2, Branch o2 c2 d2)
swapNodes (a, b) n m = if n == m then (b, a) else (a, b)

substitute :: Tree -> Tree -> Tree
substitute (Leaf X) v = v
substitute (Branch o1 a b) n = Branch o1 (substitute a n) (substitute b n)
substitute t n = t

crossoverNodes :: (RandomGen g) => (Tree, Tree) -> g -> ((Tree, Tree), g)
crossoverNodes (a,b) g = let intersectionSet = intersectionOfTreeLabels a b in
  let (crossoverPoint, g2) = randomR (0, ((length intersectionSet) - 1)) g in
    (swapNodes (a,b) 0 (elemAt crossoverPoint intersectionSet), g2)
