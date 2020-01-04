{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module SimpleIndividual
    ( individuals
    , Individual(..)
    , Gene(..)
    , crossoverListOfGenes
    , mutateListOfGenes
    ) where

import Genetic (Genetic(..))
import System.Random ( RandomGen
                     , Random(..))
import Data.Ord (Ord(..))
data Gene = A | B deriving (Show, Eq)

instance Random Gene where
  random g = let (x, y) = random g in if x then (A, y) else (B, y)
  randomR (a, b) g = random g

data Individual a = Individual {
  genes :: [Gene]
, mutationProbability :: Float
} deriving (Show, Eq)

instance (Ord a, Num a) => Ord (Individual a) where
  a `compare` b = fitness a `compare` fitness b
  (<=) a b = fitness a <= fitness b

instance (Ord a, Num a) => Genetic Individual a where
  fitness = fromIntegral . individualFitness
  mutate = mutateIndividual
  crossover = crossoverIndividual
  
-- The value of an individual Gene, for calculating fitness
geneValue :: Gene -> Int
geneValue A = 0
geneValue B = 1

individualFitness :: Individual a -> Int
individualFitness (Individual {genes = xs}) = sum $ map geneValue xs

-- could also write this as `bimap (:xs) id $ f g`
prependAndThread f (xs, g) = (\(x, g) -> (x:xs, g)) $ f g

-- Generate a list of Genes
randomGenes :: (RandomGen a) => Int -> a -> ([Gene], a)
randomGenes n g = iterate (prependAndThread random) ([], g) !! n

-- Generate a single Individual
individual :: (RandomGen g) => Int -> Float -> g -> ((Individual a), g)
individual n p g = let (xs, g2) = randomGenes n g in (Individual xs p, g2)

-- Generate a list of Individuals
individuals :: (RandomGen g) => Int -> Int -> g -> Float -> ([(Individual a)], g)
individuals n k g p = iterate (prependAndThread $ individual k p) ([], g) !! n

-- Mutate a single Gene
mutateGene :: Gene -> Gene
mutateGene A = B
mutateGene B = A

-- Mutate a single Gene in a list of Genes
-- n should be a valid index in the list
mutateListOfGenes :: [Gene] -> Int -> [Gene]
mutateListOfGenes xs n = (take n xs) ++ [mutateGene (xs !! n)] ++ (drop (n + 1) xs) where _ = xs !! n

mutateIndividual :: (RandomGen a) => (Individual b) -> a -> ((Individual b), a)
mutateIndividual Individual {genes = xs, mutationProbability = m} g =
  let (r, g2) = randomR (0, 1 :: (Float)) g in
    if r > m
    then (Individual {genes = xs, mutationProbability = m}, g2)
    else let (n, g3) = randomR (0, (length xs) - 1) g2 in
      (Individual {genes = mutateListOfGenes xs n, mutationProbability = m}, g3)

crossoverListOfGenes :: ([Gene], [Gene]) -> Int -> ([Gene], [Gene])
crossoverListOfGenes (xs, ys) n = (take n xs ++ drop n ys, take n ys ++ drop n xs)

crossoverIndividuals :: (Individual b, Individual b) -> Int -> (Individual b, Individual b)
crossoverIndividuals (Individual xs m1, Individual ys m2) n =
  let (x2s, y2s) = crossoverListOfGenes (xs, ys) n in
    (Individual x2s m1, Individual y2s m2)

crossoverIndividual :: (RandomGen a) => (Individual b, Individual b) -> a -> ((Individual b, Individual b), a)
crossoverIndividual ((Individual {genes = xs, mutationProbability = m}), b) g =
  let (crossoverPoint, g2) = randomR (0, (length xs) - 1) g
      (a2, b2) = crossoverIndividuals (Individual xs m, b) crossoverPoint in
    ((a2, b2), g2)
