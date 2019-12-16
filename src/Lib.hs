module Lib
    ( evolve
    ) where

import System.Random
import Data.Ord
import Data.List

data Gene = A | B deriving (Show,Eq)

instance Random Gene where
  random g = let (x,y) = random g in if (x == True) then (A, y) else (B, y)
  randomR (a, b) g = random g

data Individual = Individual [Gene] deriving (Show,Eq)

instance Ord Individual where
  a `compare` b = fitness a `compare` fitness b
  (<=) a b = fitness a <= fitness b

type Population = [Individual]

-- Trying to come up with a way to make the algorithm more generic
class (Ord a) => Genetic a where
  fitness :: a -> Int
  mutate :: a -> StdGen -> (a,StdGen)
  crossover :: (a,a) -> StdGen -> ((a,a),StdGen)

instance Genetic Individual where
  fitness x = individualFitness x
  mutate x g = mutateIndividual (x,g)
  crossover (x,y) g = crossoverIndividual (x,y) g
  
-- The value of an individual Gene, for calculating fitness
geneValue :: Gene -> Int
geneValue A = 0
geneValue B = 1

individualFitness :: Individual -> Int
individualFitness (Individual xs) = sum $ map geneValue xs

numberOfGenes = 10
mutationProbability = 0.1
numberOfIndividuals = 10
maxNumberOfGenerations = 5000

-- Generate a list of Genes
genes :: Int -> StdGen -> ([Gene],StdGen)
genes n g = iterate f ([], g) !! n where
  f = (\(x, g) -> let (x2, g2) = random g :: (Gene, StdGen) in (x2:x, g2))

-- Generate a single Individual
individual :: StdGen -> (Individual, StdGen)
individual g = let (xs, g2) = (genes numberOfGenes g) in (Individual xs, g2)

-- Generate a list of Individuals
individuals :: Int -> StdGen -> ([Individual], StdGen)
individuals n g = iterate f ([], g) !! n where
  f = (\(x, g) -> let (x2, g2) = individual g in (x2:x, g2))

-- Mutate a single Gene
mutateGene :: Gene -> Gene
mutateGene A = B
mutateGene B = A

-- Mutate a single Gene in a list of Genes
mutateListOfGenes :: [Gene] -> Int -> [Gene]
mutateListOfGenes xs n = (take n xs) ++ [mutateGene (xs !! n)] ++ (drop (n + 1) xs)

mutateIndividual :: (Individual, StdGen) -> (Individual, StdGen)
mutateIndividual (Individual xs, g) =
  let (r, g2) = randomR (0,1) g :: (Float, StdGen) in
    if r > 1
    then (Individual xs, g2)
    else let (n, g3) = randomR (0, (length xs) - 1) g2 :: (Int, StdGen) in
      (Individual (mutateListOfGenes xs n), g3)

crossoverListOfGenes :: ([Gene], [Gene]) -> Int -> ([Gene], [Gene])
crossoverListOfGenes (xs, ys) n = (take n xs ++ drop n ys, take n ys ++ drop n xs)

crossoverIndividuals :: (Individual, Individual) -> Int -> (Individual, Individual)
crossoverIndividuals (Individual xs, Individual ys) n =
  let (x2s, y2s) = crossoverListOfGenes (xs, ys) n in
    (Individual x2s, Individual y2s)

crossoverIndividual :: (Individual, Individual) -> StdGen -> ((Individual, Individual), StdGen)
crossoverIndividual ((Individual xs), b) g =
  let (crossoverPoint, g2) = randomR (0, length xs) g :: (Int, StdGen)
      (a2, b2) = crossoverIndividuals (Individual xs, b) crossoverPoint in
    ((a2, b2), g2)

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
stepWhile :: Population -> StdGen -> Int -> Int -> (Population, Int)
stepWhile population g generation targetFitness =
  let (xs, g2) = step (population, g)
      totalFitness = sum $ map fitness xs in
      if totalFitness >= targetFitness || generation > maxNumberOfGenerations
      then (xs, generation)
      else stepWhile xs g2 (generation + 1) targetFitness

evolve :: Int -> Int -> IO ()
evolve populationSize targetFitness = do
  initialGenerator <- getStdGen
  let (population, g2) = individuals populationSize initialGenerator
      (finalPopulation, generations) = stepWhile population g2 0 targetFitness in
    do
      putStrLn $"Generations: " ++ (show generations)
      putStrLn $ "Fitness: " ++ (show $ sum $ map fitness finalPopulation)
