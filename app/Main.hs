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

evolvePrograms :: Int ->  Double -> IO (Program ArithmeticFunction (Terminal Double))
evolvePrograms populationSize targetFitness = do
  initialGenerator <- getStdGen
  let inputsAndOutputs = [ (Constant ((-1.0) :: Double), Constant ((-0.5) :: Double))
                         , (Constant 1.0, Constant 0.5)
                         , (Constant 2.0, Constant 1.0) ]
      (population, g2) = (\(xs, g) -> (take populationSize xs, g)) $ programs (2*populationSize) initialGenerator inputsAndOutputs
      (finalPopulation, generations) = stepWhile population g2 0 targetFitness in
    do
      putStrLn $"Generations: " ++ (show generations)
      putStrLn $ show $ finalPopulation !! 0
      putStrLn $ "Fitness: " ++ (show $ fitness $ finalPopulation !! 0)
      putStrLn $ "Overall fitness: " ++ (show $ sum $ map fitness finalPopulation)
      return $ finalPopulation !! 0
