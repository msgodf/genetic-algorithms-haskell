module Lib
    ( evolve
    ) where

import System.Random
import Control.Monad
import Data.Ord
import Data.List

data Gene = A | B deriving (Show,Eq)

instance Random Gene where
  random g = let (x,y) = random g in if (x == True) then (A,y) else (B,y)
  randomR (a,b) g = random g

data Individual = Individual [Gene] deriving (Show,Eq)

instance Ord Individual where
  a `compare` b = individualFitness a `compare` individualFitness b
  (<=) a b = individualFitness a <= individualFitness b

individualFitness :: Individual -> Int
individualFitness (Individual xs) = foldr1 (+) (map (\x -> if x == A then 0 else 1) xs)

numberOfGenes = 10
mutationProbability = 0.2
numberOfIndividuals = 10
maxNumberOfGenerations = 5000

-- These functions are for generating lists of Genes, not necessarily an Individual
nextrs :: ([Gene], StdGen) -> ([Gene], StdGen)
nextrs (x, g) = let (x2, g2) = random g :: (Gene, StdGen) in (x ++ [x2], g2) 

nextn :: Int -> ([Gene], StdGen) -> ([Gene], StdGen)
nextn n = last .take n . iterate nextrs

-- These functions are for generating Individuals
individual :: StdGen -> (Individual, StdGen)
individual r = let (xs, g) = (nextn numberOfGenes ([], r)) in (Individual xs, g)

nindividuals :: ([Individual], StdGen) -> ([Individual], StdGen)
nindividuals (x,g) = let (x2,g2) = individual g in (x ++ [x2], g2)

individuals :: Int -> StdGen -> ([Individual], StdGen)
individuals n r = last $ take n $ iterate nindividuals ([], r)

mutateGene :: Gene -> Gene
mutateGene x = if x == A then B else A

mutateListOfGenes :: [Gene] -> Int -> [Gene]
mutateListOfGenes xs n = (take n xs) ++ [(mutateGene (xs!!n))] ++ (drop (n + 1) xs)

mutate :: (Individual, StdGen) -> (Individual, StdGen)
mutate (Individual xs,g) = let (r, g2) = random g :: (Float,StdGen) in
  if r > mutationProbability
  then (Individual xs,g2)
  else let (n, g3) = (randomR (0,(length xs)) g2) :: (Int,StdGen) in
    (Individual (mutateListOfGenes xs n),g3)

crossoverListOfGenes :: ([Gene], [Gene]) -> Int -> ([Gene], [Gene])
crossoverListOfGenes (xs, ys) n = (take n xs ++ drop n ys, take n ys ++ drop n xs)

crossoverIndividuals :: (Individual, Individual) -> Int -> (Individual, Individual)
crossoverIndividuals ((Individual xs), (Individual ys)) n =
  let (x2s,y2s) = crossoverListOfGenes (xs,ys) n in
    (Individual x2s, Individual y2s)

crossover :: (Individual,Individual,StdGen) -> (Individual,Individual,StdGen)
crossover (Individual xs, b, g) = let (n, g2) = (randomR (0, length xs) g) :: (Int, StdGen) in
  let (a2, b2) = crossoverIndividuals (Individual xs, b) n in (a2, b2, g2)

step :: ([Individual], StdGen) -> ([Individual], StdGen)
step (xs, g) = let alpha:beta:rest = reverse (sort xs) in
  let (gamma, delta, g2) = crossover (alpha, beta, g) in
    let (gamma2, g3) = mutate (gamma, g2) in
      let (delta2, g4) = mutate (delta, g3) in
        ([alpha] ++ [beta] ++ [gamma] ++ [delta] ++ (take ((length rest) - 2) rest), g4)

stepWhile :: ([Individual], StdGen, Int, Int) -> ([Individual],Int)
stepWhile (i,g,generation,f) =
  let (xs, g2) = step (i, g) in
    let totalFitness = foldr1 (+) (map individualFitness xs) in
      if totalFitness >= f || generation > maxNumberOfGenerations
      then (xs, generation)
      else stepWhile (xs, g2, generation + 1, f)

evolve :: Int -> Int -> IO ()
evolve k l = do
  g <- getStdGen
  let (y,g2) = individuals k g in
    let (s,generations) = (stepWhile (y, g2, 0, l)) in
      do
        putStrLn ("Generations: " ++ (show generations))
        putStrLn ("Fitness: " ++ (show (foldr1 (+) (map individualFitness s))))
