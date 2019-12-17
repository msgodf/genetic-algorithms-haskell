
module Genetic
  (
    Genetic(..),
    stepWhile
  ) where

import System.Random
import Data.Ord
import Data.List

maxNumberOfGenerations = 5000

class (Ord a) => (Genetic a) where
  fitness :: a -> Int
  mutate :: a -> StdGen -> (a,StdGen)
  crossover :: (a,a) -> StdGen -> ((a,a),StdGen)

-- A single step of the algorithm
step :: (Genetic a) => ([a], StdGen) -> ([a], StdGen)
step (xs, g) =
  let alpha:beta:rest = reverse $ sort xs
      remainingPopulation = take ((length rest) - 2) rest
      ((gamma, delta), g2) = crossover (alpha, beta) g
      (gamma2, g3) = mutate gamma g2
      (delta2, g4) = mutate delta g3 in
    (alpha:beta:gamma2:delta2:remainingPopulation, g4)

-- Run the algorithm until the target fitness is reached
stepWhile :: (Genetic a) => [a] -> StdGen -> Int -> Int -> ([a], Int)
stepWhile population g generation targetFitness =
  let (xs, g2) = step (population, g)
      totalFitness = sum $ map fitness xs in
      if totalFitness >= targetFitness || generation > maxNumberOfGenerations
      then (xs, generation)
      else stepWhile xs g2 (generation + 1) targetFitness
