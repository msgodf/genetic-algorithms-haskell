module Lib
    ( someFunc
    ) where

import System.Random
import Control.Monad

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Individual = [Integer]

individualFitness :: [Integer] -> Integer
individualFitness individual = foldr1 (+) individual


-- how to get to the Integer inside
-- For now I'm just going to pass a list of integers, perhaps data encapsulation is
-- the wrong thing to doP.

-- Generating individuals and populations
-- Need some randomness

--randomIndividual :: RandomGen g => (g, [Individual]) -> ([Integer], g)
--r :: RandomGen g => IO g -> [(Integer,g)]
--r g = iterate (\(i,f) -> (randomR (0,1) f)) (0,g)


type Gene = Bool

starts :: IO ([Gene], StdGen)
starts = getStdGen >>= return . ((,) [])


nextrs :: ([Gene], StdGen) -> IO ([Gene], StdGen)
nextrs (x,y) = let (i,r2) = random y :: (Gene,StdGen) in return (x ++ [i],r2) 

nextrs2 :: ([Gene], StdGen) -> ([Gene], StdGen)
nextrs2 (x,y) = let (i,r2) = random y :: (Gene,StdGen) in (x ++ [i],r2) 

nextn :: IO ([Gene], StdGen) -> Int -> IO ([Gene], StdGen)
nextn x n = foldl (>>=) x $ replicate n nextrs

nextn2 :: (Monad m) => Int -> ([Gene], StdGen) -> m ([Gene], StdGen)
nextn2 n = return . last .take n . iterate nextrs2

--nextrs :: ([Int], StdGen) -> IO ([Int], StdGen)
--nextrs (x,y) = let (i,r2) = random y :: (Int,StdGen) in return (x ++ [i],r2) 

--fff = return $ (replicateM 10 nextrs)

--fff = do
--  x <- starts
--  starts >>= (replicateM 10 nextrs) :: (IO ([Int], StdGen))

-- I want a function that takes a RandomGen and produces a list of n integers and the rest of the generator


--a = getStdGen >>= randomIndividual
numberOfGenes = 10

individual :: StdGen -> IO ([Gene],StdGen)
individual r = (return . ((,) [])) r >>= nextn2 numberOfGenes

--inindividualo :: StdGen -> ([Gene],StdGen)
individualo :: Monad m => StdGen -> m ([Gene],StdGen)
individualo r = nextn2 numberOfGenes ([],r)

-- Can call individualo like this
-- getStdGen >>= individualo
-- it needs to be completely non-monadic though for me to use iterate on it
-- which means getting a non-monadic nextn2
--nextn3 :: Int -> ([Gene], StdGen) -> ([Gene], StdGen)
nextn3 n = last .take n . iterate nextrs2


--individualt :: StdGen -> ([Gene],StdGen)
individualt r = (return . ((,) [])) r >>= nextn3 numberOfGenes

-- individualt <$> getStdGen
-- what is this equivalent to?
-- it's infix fmap
-- fmap individualt getStdGen

-- Don't really need to consider IO except for at the outside when we get the generator



-- generate a list of random individuals


-- need to keep passing the RandomGen through

-- individuals would take an IO StdGen and produce an IO ([[Gene]], StdGen)
--nindividuals :: ([[Gene]],StdGen) -> IO ([[Gene]],StdGen)
nindividuals :: ([[Gene]], StdGen) -> IO ([[Gene]], StdGen)
nindividuals (x,y) = do
  (i,r2) <- individual y
  return (x ++ [i],r2)

nindividualso :: ([[Gene]], StdGen) -> ([[Gene]], StdGen)
nindividualso (x,y) = let (a,b) = individualt y in (x++[a],b)

individuals :: Monad m => Int -> m StdGen -> m ([[Gene]],StdGen)
individuals n r = fmap (last . take n . iterate nindividualso) (r >>= (return . (,) ([]::[[Gene]])))

-- I wonder whether this is even the nicest way, and whether avoiding the monad whenever possible is better
individuals2 :: Int -> StdGen -> ([[Gene]],StdGen)
individuals2 n r = (last . take n . iterate nindividualso) ([]::[[Gene]],r)



-- We only need to call fmap when we are getting an IO something back and need to pass the something to a function



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
