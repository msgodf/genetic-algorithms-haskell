module Main where
import SimpleIndividual
import Genetic
import System.Random

main :: IO ()
main = evolve 20 182

evolve :: Int -> Int -> IO ()
evolve populationSize targetFitness = do
  initialGenerator <- getStdGen
  let (population, g2) = individuals populationSize initialGenerator
      (finalPopulation, generations) = stepWhile population g2 0 targetFitness in
    do
      putStrLn $"Generations: " ++ (show generations)
      putStrLn $ "Fitness: " ++ (show $ sum $ map fitness finalPopulation)

