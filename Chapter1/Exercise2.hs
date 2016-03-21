{-
Implement a simple GA with fitness−proportionate selection, roulettewheel sampling,
population size 100, single−point crossover rate pc = 0.7, and bitwise mutation
rate pm = 0.001. Try it on the fitness function ƒ(x) = the integer represented by
the binary number x, where x is a chromosome of length 20. Run the GA for 100
generations and plot the fitness of the best individual found at each generation
as well as the average fitness of the population at each generation. How do these
plots change as you vary the population size, the crossover rate, and the mutation
rate? What if you use only mutation (i.e., pc = 0)?
-}
{-# LANGUAGE TupleSections #-}
module Exercise2 where

import System.Random

import Debug.Trace


type Chromo = [Bool]

popSize = 100
chromoSize = 20
pCrossover = 0.7
pMutation = 0.001


main :: IO ()
main = do
    ((result, _), n) <- until solved evolve' . (,0) . initPop <$> getStdGen
    return ()

solved :: (([Chromo], StdGen), Int) -> Bool
solved ((_, _), n) = n > 100

evolve' :: (([Chromo], StdGen), Int) -> (([Chromo], StdGen), Int)
evolve' (current, n) = trace (show bestFitness ++ " " ++ show avgFitness)
    $ (evolve current, n + 1)
  where
    totalFitness = sum allFitnesses
    allFitnesses = map fitness $ fst current
    avgFitness   = (fromIntegral totalFitness) / (fromIntegral $ length $ fst current)
    bestFitness  = maximum allFitnesses

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
fitness = fromBinary

fromBinary :: [Bool] -> Int
fromBinary = go 0
  where
    go n [] = n
    go n (d:ds) = go (2 * n + if d then 1 else 0) ds

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

