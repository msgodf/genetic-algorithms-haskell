module Tree
  ( trees
  , evaluate
  , substitute
  , Tree(..)
  , Terminal(..)
  , Function(..)
  ) where

import Genetic (Genetic(..))
import System.Random
import Data.Ord (Ord(..))
import Data.Set ( Set
                , elemAt
                , intersection
                , union
                , singleton
                , empty)

data Function = Add | Subtract | Multiply | Divide deriving (Show, Eq)

maximumTreeDepth = 8
mutationProbability = 0.1
programLengthFitnessWeighting = 1.0
targets = [(-10,-10),(0,0),(10,10)]

instance (Eq a, Num a, Fractional a, Random a, Example a) => Ord (Tree a) where
  a `compare` b = fitness a `compare` fitness b
  (<=) a b = fitness a <= fitness b

instance (Eq a, Num a, Fractional a, Random a, Example a) => Genetic (Tree a) where
  fitness x = 0 :: Double --programFitnessOverInputs examples
  mutate = subtreeMutation mutationProbability
  crossover = crossoverNodes

class (Fit a) => (Example a) where
  examples :: [(a,a)]

instance Example Double where
  examples = targets

class (Fit a) where
  difference :: a -> a -> a

instance Fit Double where
  difference x y = abs (x - y)

instance Random Function where
  random g = let (x, g2) = randomR (0, 3 :: Int) g in ([Add, Subtract, Multiply, Divide] !! x, g2)
  randomR _ g = random g

data Terminal a = Constant a | X deriving (Show, Eq)

data Tree a = Leaf (Terminal a) | Branch Function (Tree a) (Tree a) deriving (Show, Eq)

data Program a = Program (Tree a)

-- how can I get the operation into this?
-- so that I could implement evaluate
-- evaluate isn't really a use of fold, because the function is the operation
-- perhaps, it is, perhaps f could apply o to l and r if they're both leaves

instance Foldable Tree where
  foldr f z (Leaf (Constant x)) = f x z
  foldr f z (Branch o l r) = foldr f (foldr f (foldr f z r) l) o

--  foldMap f (Leaf (Constant x)) = f x 
--  foldMap f (Branch o l r) = foldMap f l `mappend` foldMap f r
  
instance (Num a, Random a) => Random (Tree a) where
  random = randomTree
  randomR _ = random

maxOr0 [] = 0
maxOr0 xs = maximum xs

-- How to get this function to always return a double (because I don't want to push the genericity all the way up to the user)
programFitnessOverInputs :: (Num a, Fractional a, Fit a) => [(a, a)] -> (Tree a) -> a
programFitnessOverInputs xs x = case pp of
                                  (Leaf (Constant ll)) -> difference ll (snd (xs !! 0))
  where pp = evaluate $ (substitute x (Leaf (Constant v))) where v = fst (xs !! 0)


--(sum $
                               --   fmap (\(input, output) -> let (Leaf (Constant v)) = evaluate $
                                      --                            substitute x (Leaf (Constant input)) in
                                        --                      abs(output - v))
                                 -- xs)
                                --- programLengthFitnessWeighting*(fromIntegral $ treeSize x)

randomTree :: (RandomGen g, Num a, Random a) => g -> ((Tree a), g)
randomTree g = (\(x, _, z) -> (x, z)) $ f 0 g where
  f = (\d g -> if d >= maximumTreeDepth
               then
                 let (value, g2) = random g in
                   (Leaf (Constant value), d + 1, g2)
               else
                 let (x, g2) = randomR (0, 2 :: Int) g in
                   case x of
                     0 -> let (value, g3) = random g2 in
                       (Leaf (Constant value), d, g3)
                     1 -> let (operation, g3) = random g2
                              (left, d2, g4) = f (d + 1) g3
                              (right, d3, g5) = f (d + 1) g4 in
                            ((Branch operation left right), d3, g5)
                     2 -> (Leaf X, d, g2))

prependAndThread f (xs, g) = (\(x, g) -> if (containsVariables x) then (x:xs, g) else (xs, g)) $ f g

trees :: (RandomGen g, Num a, Random a) => Int -> g -> ([(Tree a)], g)
trees n g = iterate (prependAndThread randomTree) ([],g) !! n

-- Operate can only operate on values, not variables
operate :: (Num a, Fractional a) => Function -> (Terminal a) -> (Terminal a) -> (Terminal a) 
operate Add (Constant x) (Constant y) = (Constant (x + y))
operate Subtract (Constant x) (Constant y) = (Constant (x - y))
operate Multiply (Constant x) (Constant y) = (Constant (x * y))
operate Divide (Constant x) (Constant y) = (Constant (x / y)) --if x == 0 || y == 0 || x == y then (Constant 1e18) else (Constant (x / y))

evaluate :: (Num a, Fractional a) => (Tree a) -> (Tree a)
evaluate (Leaf a) = (Leaf a)
evaluate (Branch o (Leaf a) (Leaf b)) = Leaf $ operate o a b
evaluate (Branch o a b) = evaluate $ Branch o (evaluate a) (evaluate b)

flipBranches :: (Tree a) -> (Tree a)
flipBranches (Branch o a b) = Branch o b a
flipBranches (Leaf a) = Leaf a

treeDepth :: (Tree a) -> Int
treeDepth t = f t 0 where
  f = (\t d -> case t of (Branch o a b) -> if m > n
                                           then m
                                           else n where m = f a (d + 1)
                                                        n = f b (d + 1)
                         (Leaf a) -> d)

labelTree :: (Tree a) -> Set Int
labelTree t = f t 0 empty where
  f = (\t n xs -> case t of (Leaf a) -> (singleton n) `union` xs
                            (Branch o a b) -> (singleton n) `union`
                                              (f a (2*n + 1) xs) `union`
                                              (f b (2*n + 2) xs))

intersectionOfTreeLabels :: (Tree a) -> (Tree a) -> Set Int
intersectionOfTreeLabels t1 t2 = (labelTree t2) `intersection` (labelTree t1)

swapNodes :: ((Tree a), (Tree a)) -> Int -> ((Tree a), (Tree a))
swapNodes ts n = f ts n 0 where
  f = (\ts n m -> case ts of
          (Branch o1 a b, Branch o2 c d) -> let (a2, c2) = f (a, c) (2*n + 1) m
                                                (b2, d2) = f (b, d) (2*n + 2) m in
                                              if n == m
                                              then (Branch o2 c2 d2, Branch o1 a2 b2)
                                              else (Branch o1 a2 b2, Branch o2 c2 d2)
          (a, b) -> if n == m then (b, a) else (a, b))

replaceFirstLeafWithVariable :: (Tree a) -> (Tree a)
replaceFirstLeafWithVariable t = fst $ f t where
  f = (\t -> case t of
          (Leaf (Constant t)) -> ((Leaf X), True)
          (Branch o a b) -> let (t1, r1) = f a
                                (t2, r2) = f b in
                              if r1
                              then (Branch o t1 b, r1)
                              else
                                if r2
                                then (Branch o a t2, r2)
                                else (Branch o a b, False)
          t -> (t, False))

substituteNthNode :: (Tree a) -> (Tree a) -> Int -> (Tree a)
substituteNthNode t1 t2 n = f t1 t2 n 0 where
  f = (\t1 t2 n m -> case t1 of
          (Leaf a) -> if n == m then t2 else (Leaf a)
          (Branch o a b) -> if n == m then t2
                            else
                              if m > n
                              then Branch o a b
                              else Branch o (f a t2 n (2*m + 1)) (f b t2 n (2*m + 2)))

subtreeMutation :: (RandomGen g, Num a, Random a) => Double -> (Tree a) -> g -> ((Tree a), g)
subtreeMutation p t g  = let (r, g2) = randomR (0, 1 :: (Double)) g in
                          if r > p
                          then (t, g2)
                          else let labels = labelTree t
                                   (p, g3) = randomR (0, length labels - 1) g2
                                   n = elemAt p labels
                                   (t2, g4) = randomTree g3 in
                            (substituteNthNode t t2 n, g4)

substitute :: (Tree a) -> (Tree a) -> (Tree a)
substitute (Leaf X) v = v
substitute (Branch o a b) n = Branch o (substitute a n) (substitute b n)
substitute t n = t

crossoverNodes :: (RandomGen g) => ((Tree a), (Tree a)) -> g -> (((Tree a), (Tree a)), g)
crossoverNodes (a, b) g = let intersectionSet = intersectionOfTreeLabels a b
                              (crossoverPoint, g2) = randomR (0, ((length intersectionSet) - 1)) g in
                           (swapNodes (a,b) (elemAt crossoverPoint intersectionSet), g2)

containsVariables :: (Tree a) -> Bool
containsVariables (Branch o a b) = containsVariables a || containsVariables b
containsVariables (Leaf X) = True
containsVariables x = False

treeSize :: (Tree a) -> Int
treeSize x = length $ labelTree x

