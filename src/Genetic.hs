{-# LANGUAGE MultiParamTypeClasses #-}
module Genetic
  ( Genetic(..)
  , stepWhile
  ) where

import System.Random (RandomGen)
import Data.Ord (Ord(..))
import Data.List (sort)

maxNumberOfGenerations = 5000

class (Ord (m a), Ord a) => Genetic m a where
  fitness :: m a -> a
  mutate :: (RandomGen b) => m a -> b -> (m a, b)
  crossover :: (RandomGen b) => ((m a),(m a)) -> b -> (((m a),(m a)), b)

selection :: (Genetic m a) => [m a] -> ([m a], [m a])
selection xs = (alpha:beta:[], rest) where
  alpha:beta:rest = reverse $ sort xs

-- A single step of the algorithm
step :: (Genetic m a, RandomGen c) => ([m a], c) -> ([m a], c)
step (xs, g) =
  let
      (alpha:beta:_, rest) = selection xs
      remainingPopulation = take ((length rest) - 2) rest
      ((gamma, delta), g2) = crossover (alpha, beta) g
      (gamma2, g3) = mutate gamma g2
      (delta2, g4) = mutate delta g3 in
    (alpha:beta:gamma2:delta2:remainingPopulation, g4)

-- Run the algorithm until the target fitness is reached
stepWhile :: (Genetic m a, RandomGen c) => [(m a)] -> c -> Int -> a -> ([(m a)], Int)
stepWhile population g generation targetFitness =
  let (xs, g2) = step (population, g)
      totalFitness = fitness (xs !! 0) in
      if totalFitness >= targetFitness || generation > maxNumberOfGenerations
      then (xs, generation)
      else stepWhile xs g2 (generation + 1) targetFitness

