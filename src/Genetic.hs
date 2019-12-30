module Genetic
  ( Genetic(..)
  , stepWhile
  ) where

import System.Random (RandomGen)
import Data.Ord (Ord(..))
import Data.List (sort)

maxNumberOfGenerations = 5000

class (Ord a) => (Genetic a) where
  fitness :: a -> Double
  mutate :: (RandomGen b) => a -> b -> (a, b)
  crossover :: (RandomGen b) => (a,a) -> b -> ((a,a), b)

selection :: (Genetic a) => [a] -> ([a], [a])
selection xs = (alpha:beta:[], rest) where
  alpha:beta:rest = reverse $ sort xs

-- A single step of the algorithm
step :: (Genetic a, RandomGen b) => ([a], b) -> ([a], b)
step (xs, g) =
  let
      (alpha:beta:_, rest) = selection xs
      remainingPopulation = take ((length rest) - 2) rest
      ((gamma, delta), g2) = crossover (alpha, beta) g
      (gamma2, g3) = mutate gamma g2
      (delta2, g4) = mutate delta g3 in
    (alpha:beta:gamma2:delta2:remainingPopulation, g4)

-- Run the algorithm until the target fitness is reached
stepWhile :: (Genetic a, RandomGen b) => [a] -> b -> Int -> Double -> ([a], Int)
stepWhile population g generation targetFitness =
  let (xs, g2) = step (population, g)
      totalFitness = fitness (xs !! 0) in
      if totalFitness >= targetFitness || generation > maxNumberOfGenerations
      then (xs, generation)
      else stepWhile xs g2 (generation + 1) targetFitness

