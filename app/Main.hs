module Main where
import SimpleIndividual
import Genetic
import System.Random

main :: IO ()
main = evolve 20 10 182

evolve :: Int -> Int -> Double -> IO ()
evolve populationSize numberOfGenes targetFitness = do
  initialGenerator <- getStdGen
  let (population, g2) = individuals populationSize numberOfGenes initialGenerator 0.3
      (finalPopulation, generations) = stepWhile population g2 0 targetFitness in
    do
      putStrLn $"Generations: " ++ (show generations)
      putStrLn $ "Fitness: " ++ (show $ sum $ map fitness finalPopulation)

