module Main where

import SimpleIndividual
import Tree
import Genetic
import System.Random

main :: IO ()
main = evolve 20 10 182.0

evolve :: Int -> Int -> Double -> IO ()
evolve populationSize numberOfGenes targetFitness = do
  initialGenerator <- getStdGen
  let (population, g2) = individuals populationSize numberOfGenes initialGenerator 0.3
      (finalPopulation, generations) = stepWhile population g2 0 targetFitness in
    do
      putStrLn $"Generations: " ++ (show generations)
      putStrLn $ "Fitness: " ++ (show $ sum $ map fitness finalPopulation)

evolvePrograms :: Int -> IO (Program ArithmeticFunction (Terminal Double))
evolvePrograms populationSize = do
  initialGenerator <- getStdGen
  let inputsAndOutputs =  [(Constant x, Constant (x*x)) | x <- [-10..10]]
      (population, g2) = (\(xs, g) -> (take populationSize xs, g)) $ programs (2*populationSize) initialGenerator inputsAndOutputs
      (finalPopulation, generations) = stepWhile population g2 0 (-3.0) in
    do
      putStrLn $"Generations: " ++ (show generations)
      putStrLn $ "Fitness: " ++ (show $ fitness $ finalPopulation !! 0)
      return $ finalPopulation !! 0
