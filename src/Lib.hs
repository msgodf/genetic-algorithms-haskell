module Lib
    ( someFunc
    ) where

import System.Random
import Control.Monad
import Data.Ord

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Gene = A | B deriving (Show,Eq)
instance Random Gene where
  random g = let (x,y) = random g in if (x == True) then (A,y) else (B,y)
  randomR (a,b) g = random g

data Individual = Individual [Gene] deriving (Show,Eq)

-- If an individual is just a list of Gene, is it better to encapsulate or not?
-- i.e. making it opaque

instance Ord Individual where
  a `compare` b = (individualFitness a) `compare` (individualFitness b)
  (<=) a b = (individualFitness a) <= (individualFitness b)

individualFitness :: Individual -> Int
individualFitness (Individual xs) = foldr1 (+) (map (\x -> if x == A then 0 else 1) xs)

--foldr1 (+) individual

-- how to get to the Integer inside
-- For now I'm just going to pass a list of integers, perhaps data encapsulation is
-- the wrong thing to doP.

-- Generating individuals and populations
-- Need some randomness


numberOfGenes = 10
numberOfIndividuals = 10

nextrs :: ([Gene], StdGen) -> ([Gene], StdGen)
nextrs (x,g) = let (x2, g2) = random g :: (Gene,StdGen) in (x ++ [x2], g2) 

nextn :: Int -> ([Gene], StdGen) -> ([Gene], StdGen)
nextn n = last .take n . iterate nextrs

individual :: StdGen -> ([Gene],StdGen)
individual r = nextn 10 ([],r)

nindividuals :: ([[Gene]], StdGen) -> ([[Gene]], StdGen)
nindividuals (x,g) = let (x2,g2) = individual g in (x ++ [x2], g2)

individuals :: Int -> StdGen -> ([[Gene]],StdGen)
individuals n r = last $ take n $ iterate nindividuals ([], r)

-- need to sort a list of individuals by their fitness

-- What is an Individual?
-- a random sequence of n genes
-- a fitness function

-- What is a Population?
-- a set of individuals
-- an ordering of that set by Individual fitness

--  selection step

-- find the first and second fittest individual in
-- a population


-- crossover step
-- choose a crossover point m in [0,n]
-- create two new offspring,
-- 1) by taking the first m genes of the first parent and the remaining n-m genes of the second parent
-- 2) the first m genes of the second parent and the remaining n-m genes of the first parent

-- the parents are changed in place, or are replaced by their offspring

-- with some probability one of the genes in both of the two offspring is flipped  (the choice of gene is independent in each offspring)
