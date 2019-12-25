module Tree where

import Genetic (Genetic(..))
import System.Random
import Data.Ord (Ord(..))
import Data.Set

data Operation = Add | Subtract | Multiply | Divide deriving (Show,Eq)

maximumTreeDepth = 10

instance Ord Tree where
  a `compare` b = fitness a `compare` fitness b
  (<=) a b = fitness a <= fitness b

instance Genetic Tree where
  fitness x = let (Leaf v) = evaluate x in v
  mutate x g = (x, g)
  crossover (x,y) g = ((x,y),g)

instance Random Operation where
  random g = let (x, g2) = randomR (0, 3 :: Int) g in ([Add, Subtract, Multiply, Divide] !! x, g2)
  randomR _ g = random g
  
type Value = Double

data Tree = Leaf Value | Branch Operation Tree Tree deriving (Show,Eq)

instance Random Tree where
  random g = (\(x,_,y) -> (x, y)) $ randomTree 0 g
  randomR _ g = random g

randomTree ::(RandomGen g) => Int -> g -> (Tree, Int, g)
randomTree d g = if d >= maximumTreeDepth
  then
  let (value,g2) = random g in
        (Leaf value, d + 1, g2)
  else
  let (x, g2) = randomR (0, 1::Int) g in
    case x of
      0 -> let (value, g3) = random g2 in
        (Leaf value, d, g3)
      1 -> let (operation, g3) = random g2
               (left, d2, g4) = randomTree (d + 1) g3
               (right, d3, g5) = randomTree (d + 1) g4 in
             ((Branch operation left right), d3, g5)

operate :: Operation -> Value -> Value -> Value
operate Add x y = x + y
operate Subtract x y = x - y
operate Multiply x y = x * y
operate Divide x y = x / y

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
