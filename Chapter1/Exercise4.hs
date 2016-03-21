{-
Compare the GA's performance on the fitness functions of computer exercises
1 and 2 with that of steepest−ascent hill climbing (defined above) and with that
of another simple hill−climbing method, "random−mutation hill climbing" (Forrest
and Mitchell 1993b):

1. Start with a single randomly generated string. Calculate its fitness.

2. Randomly mutate one locus of the current string.

3. If the fitness of the mutated string is equal to or higher than the fitness of
the original string, keep the mutated string. Otherwise keep the original string.

4. Go to step 2.

Iterate this algorithm for 10,000 steps (fitness−function evaluations). This is
equal to the number of fitness−function evaluations performed by the GA in computer
exercise 2 (with population size 100 run for 100 generations). Plot the best fitness
found so far at every 100 evaluation steps (equivalent to one GA generation),
averaged over 10 runs. Compare this with a plot of the GA's best fitness found so
far as a function of generation. Which algorithm finds higher−fitness chromosomes?
Which algorithm finds them faster? Comparisons like these are important if claims
are to be made that a GA is a more effective search algorithm than other stochastic
methods on a given problem.
-}
{-# LANGUAGE TupleSections #-}
module Exercise4 where

import System.Random

import Debug.Trace


type Chromo = [Bool]


main :: IO ()
main = do
    ((result, _), n) <- until solved step' . (,0) . initPop <$> getStdGen
    return ()

solved :: ((Chromo, StdGen), Int) -> Bool
solved ((_, _), n) = n > 10000

step' :: ((Chromo, StdGen), Int) -> ((Chromo, StdGen), Int)
step' (current@(c, _), n) = t (step current, n + 1)
  where
    t = if n `div` 100 == 0 then traceShow (fitness c) else id

step :: (Chromo, StdGen) -> (Chromo, StdGen)
step (c, g)
    | fitness new > fitness c = (new, g')
    | otherwise = (c, g')
  where
    (new, g') = mutate g c

initPop :: StdGen -> (Chromo, StdGen)
initPop g = iterate addValue ([], g) !! 20
  where
    addValue (existing, g) = let (new, g') = random g in (new : existing, g')

fitness :: Chromo -> Int
fitness = fromBinary

fromBinary :: [Bool] -> Int
fromBinary = go 0
  where
    go n [] = n
    go n (d:ds) = go (2 * n + if d then 1 else 0) ds

mutate :: StdGen -> Chromo -> (Chromo, StdGen)
mutate g c = (take locus c ++ (new : drop (locus + 1) c), g'')
  where
    (locus, g') = randomR (0, length c - 1) g
    (new, g'')  = random g'

