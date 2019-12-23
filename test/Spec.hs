module Main where

import Genetic (Genetic(..))
import SimpleIndividual

import Test.QuickCheck

import System.Random

instance Arbitrary Gene where
  arbitrary = choose (A, B)

instance Arbitrary Individual where
  arbitrary = Individual <$> arbitrary <*> arbitrary

main = sequence [ quickCheck prop_FitnessOfEmptyIndividualIsZero
                , quickCheck prop_CrossoverMaintainsTotalLength
                , quickCheck prop_MaxFitness
                , quickCheck prop_MutateMaintainsLength]

prop_MaxFitness xs = fitness (Individual xs 0) <= length xs

prop_FitnessOfEmptyIndividualIsZero = fitness (Individual [] 0) == 0

prop_CrossoverMaintainsTotalLength x y z =
  let (x2,y2) = crossoverListOfGenes (x,y) z in length x + length y == length x2 + length y2

-- Ignores cases that will result in an exception being thrown by xs !! n
prop_MutateMaintainsLength xs n = xs == [] || n >= length xs || n < 0 || length (mutateListOfGenes xs n) == length xs

