module Main where
import SimpleIndividual
import Tree
import Genetic
import System.Random

main :: IO ()
main = putStrLn "Hi" -- evolvePrograms 20 0.0  --182

--evolve :: Int -> Int -> Double -> IO ()
--evolve populationSize numberOfGenes targetFitness = do
--  initialGenerator <- getStdGen
--  let (population, g2) = individuals populationSize numberOfGenes initialGenerator 0.3
--      (finalPopulation, generations) = stepWhile population g2 0 targetFitness in
--    do
--      putStrLn $"Generations: " ++ (show generations)
--      putStrLn $ "Fitness: " ++ (show $ sum $ map fitness finalPopulation)

evolvePrograms :: Int ->  Double -> IO ()--(Tree ArithmeticFunction Double)
evolvePrograms populationSize targetFitness = do
  initialGenerator <- getStdGen
  let (population, g2) = (getPopulation populationSize initialGenerator) :: ([Tree ArithmeticFunction Double],StdGen)
      (finalPopulation, generations) = stepWhile population g2 0 targetFitness in
    do
      putStrLn $"Generations: " ++ (show generations)
--      putStrLn $ show $ finalPopulation !! 0
--      putStrLn $ "Fitness: " ++ (show $ fitness (finalPopulation !! 0))
--      putStrLn $ "Overall fitness: " ++ (show $ sum $ map fitness finalPopulation)
--      return 1


getPopulation :: (Num b, Random (a b), Random b, RandomGen g) => Int -> g -> ([Tree a b], g)
getPopulation n g = (\(xs, g) -> (take n xs, g)) $ theTrees n g

theTrees :: (Num b, Random (a b), Random b, RandomGen g) => Int -> g -> ([Tree a b], g)
theTrees n g = trees (2*n) g
