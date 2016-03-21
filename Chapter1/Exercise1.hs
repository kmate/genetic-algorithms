{-
Implement a simple GA with fitness−proportionate selection, roulettewheel
sampling, population size 100, single−point crossover rate pc = 0.7, and bitwise
mutation rate pm = 0.001. Try it on the following fitness function: ƒ(x) = number
of ones in x, where x is a chromosome of length 20. Perform 20 runs, and measure
the average generation at which the string of all ones is discovered. Perform the
same experiment with crossover turned off (i.e., pc = 0). Do similar experiments,
varying the mutation and crossover rates, to see how the variations affect the
average time required for the GA to find the optimal string. If it turns out that
mutation with crossover is better than mutation alone, why is that the case?
-}
{-# LANGUAGE TupleSections #-}
module Exercise1 where

import System.Random


type Chromo = [Bool]

popSize = 100
chromoSize = 20
pCrossover = 0.7
pMutation = 0.001


main :: IO ()
main = do
    ((result, _), n) <- until solved evolve' . (,0) . initPop <$> getStdGen
    print n

solved :: (([Chromo], StdGen), Int) -> Bool
solved ((pop, _), n) = any and pop

evolve' :: (([Chromo], StdGen), Int) -> (([Chromo], StdGen), Int)
evolve' (current, n) = (evolve current, n + 1)

evolve :: ([Chromo], StdGen) -> ([Chromo], StdGen)
evolve (pop, g) = until ((>= popSize) . length . fst) addChromo ([], g)
  where
    addChromo (pop', g) = (mutatedOffsprings ++ pop', g''')
      where
        (mutatedOffsprings, g''') =
          let (mutated1, gm1) = mutate g'' offspring1
              (mutated2, gm2) = mutate gm1 offspring2
          in  ([mutated1, mutated2], gm2)
        ((offspring1, offspring2), g'') = crossOver g' selectedParents
        (selectedParents, g') = selectParents g pop 

selectParents :: StdGen -> [Chromo] -> ((Chromo, Chromo), StdGen)
selectParents g pop = ((parent1, parent2), g'')
  where
    (parent1, g')  = select g
    (parent2, g'') = select g'
    select g =
      let (ball, g') = randomR (0, totalFitness - 1) g
      in (roulette pop allFitnesses ball, g')
    totalFitness = sum allFitnesses
    allFitnesses = map fitness pop

roulette :: [Chromo] -> [Int] -> Int -> Chromo
roulette pop allFitnesses ball = fst $ withFitness !! snd (until reached step (snd $ head withFitness, 0))
  where
    reached (value, _) = value >= ball
    step (value, index) = (value + snd (withFitness !! (index + 1)), index + 1)
    withFitness = zip pop allFitnesses

initPop :: StdGen -> ([Chromo], StdGen)
initPop g = iterate addChromo ([], g) !! popSize
  where
    addChromo (existing, g) = let (new, g') = newChromo g in (new : existing, g')
    newChromo g = iterate addValue ([], g) !! chromoSize
    addValue (existing, g) = let (new, g') = random g in (new : existing, g')

fitness :: Chromo -> Int
fitness = length . filter (==True)

crossOver :: StdGen -> (Chromo, Chromo) -> ((Chromo, Chromo), StdGen)
crossOver g (a, b)
  | doIt <= pCrossover = (( take locus a ++ drop locus b
                          , take locus b ++ drop locus a ), g'')
  | otherwise = ((a, b), g'')
  where
    (doIt,  g')  = random g :: (Double, StdGen)
    (locus, g'') = randomR (0, length a - 1) g'

mutate :: StdGen -> Chromo -> (Chromo, StdGen)
mutate g = foldl processGene ([], g)
  where
    processGene (processed, g) current =
      let (doIt, g') = random g :: (Double, StdGen)
          (new, g'') =
            if doIt <= pMutation
              then random g'
              else (current, g')
      in (new : processed, g'')

