module SimpleIndividual
    ( individuals
    ) where

import Genetic (Genetic(..))
import System.Random ( RandomGen
                     , Random(..))
import Data.Ord (Ord(..))
data Gene = A | B deriving (Show,Eq)

instance Random Gene where
  random g = let (x,y) = random g in if x then (A, y) else (B, y)
  randomR (a, b) g = random g

data Individual = Individual {
  genes :: [Gene]
, mutationProbability :: Float
} deriving (Show, Eq)

-- It's interesting that the generic part is the Individual, but I wonder whether mutation probability and choice of crossover point should be a property of an individual.

-- Trying to come up with a way to make the algorithm more generic
instance Ord Individual where
  a `compare` b = fitness a `compare` fitness b
  (<=) a b = fitness a <= fitness b

instance Genetic Individual where
  fitness x = individualFitness x
  mutate x g = mutateIndividual (x,g)
  crossover (x,y) g = crossoverIndividual (x,y) g
  
-- The value of an individual Gene, for calculating fitness
geneValue :: Gene -> Int
geneValue A = 0
geneValue B = 1

individualFitness :: Individual -> Int
individualFitness (Individual {genes=xs}) = sum $ map geneValue xs

numberOfGenes = 10 :: Int

-- could also write this as `bimap (:xs) id $ f g`
prependAndThread f (xs, g) = (\(x, g) -> (x:xs, g)) $ f g

-- Generate a list of Genes
randomGenes :: (RandomGen a) => Int -> a -> ([Gene],a)
randomGenes n g = iterate (prependAndThread random) ([], g) !! n where

-- Generate a single Individual
individual :: (RandomGen a) => Int -> Float -> a -> (Individual, a)
individual n p g = let (xs, g2) = randomGenes n g in (Individual {genes=xs,mutationProbability=p}, g2)

-- Generate a list of Individuals
individuals :: (RandomGen a) => Int -> Int -> a -> Float -> ([Individual], a)
individuals n k g p = iterate (prependAndThread $ individual k p) ([],g) !! n

-- Mutate a single Gene
mutateGene :: Gene -> Gene
mutateGene A = B
mutateGene B = A

-- Mutate a single Gene in a list of Genes
mutateListOfGenes :: [Gene] -> Int -> [Gene]
mutateListOfGenes xs n = (take n xs) ++ [mutateGene (xs !! n)] ++ (drop (n + 1) xs)

mutateIndividual :: (RandomGen a) => (Individual, a) -> (Individual, a)
mutateIndividual (Individual {genes=xs,mutationProbability=m}, g) =
  let (r, g2) = randomR (0,1 :: (Float)) g in
    if r > m
    then (Individual {genes=xs,mutationProbability=m}, g2)
    else let (n, g3) = randomR (0, (length xs) - 1) g2 in
      (Individual {genes=(mutateListOfGenes xs n),mutationProbability=m}, g3)

crossoverListOfGenes :: ([Gene], [Gene]) -> Int -> ([Gene], [Gene])
crossoverListOfGenes (xs, ys) n = (take n xs ++ drop n ys, take n ys ++ drop n xs)

crossoverIndividuals :: (Individual, Individual) -> Int -> (Individual, Individual)
crossoverIndividuals (Individual {genes=xs,mutationProbability=m1}, Individual {genes=ys, mutationProbability=m2}) n =
  let (x2s, y2s) = crossoverListOfGenes (xs, ys) n in
    (Individual {genes=x2s,mutationProbability=m1}, Individual {genes=y2s,mutationProbability=m2})

crossoverIndividual :: (RandomGen a) => (Individual, Individual) -> a -> ((Individual, Individual), a)
crossoverIndividual ((Individual {genes=xs,mutationProbability=m}), b) g =
  let (crossoverPoint, g2) = randomR (0, length xs) g
      (a2, b2) = crossoverIndividuals (Individual {genes=xs,mutationProbability=m}, b) crossoverPoint in
    ((a2, b2), g2)
