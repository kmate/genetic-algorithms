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
module Chapter1.Exercise4 where

import Control.Monad.State
import System.Random

import Debug.Trace


type Fitness = Int
type Chromo  = [Bool]
data GState
  = GState
  { generator  :: StdGen
  , generation :: Int
  , solution   :: Chromo
  } deriving Show
type G = State GState

runG :: StdGen -> (Chromo, Int, StdGen)
runG gen = (solution finish, generation finish, generator finish)
  where
    finish = execState evolve start
    start = GState
          { generator  = gen
          , generation = 0
          , solution   = undefined
          }

randomG :: Random a => G a
randomG = state $ \s ->
    let (x, g') = random (generator s)
    in  (x, s { generator = g' })

randomGR :: Random a => (a, a) -> G a
randomGR range = state $ \s ->
    let (x, g') = randomR range (generator s)
    in  (x, s { generator = g' })

evolve :: G ()
evolve = do
    initPopulation
    let step = do
            done <- solved
            unless done $ do
                current <- gets solution
                mutated <- mutate current
                let oldFitness = fromBinary current
                    newFitness = fromBinary mutated
                n <- gets generation
                modify $ \s -> s
                    { generation = generation s + 1
                    , solution   = if newFitness > oldFitness
                                        then mutated
                                        else current }
                let t = if n `div` 100 == 0 then traceShow oldFitness else id
                t step
    step

initPopulation :: G ()
initPopulation = do
    newInstance <- do
        let chromoSize = 20
        replicateM chromoSize randomG
    modify $ \s -> s { solution = newInstance }

solved :: G Bool
solved = (>= 10000) <$> gets generation

fromBinary :: Chromo -> Int
fromBinary = go 0
  where
    go n [] = n
    go n (d:ds) = go (2 * n + if d then 1 else 0) ds

mutate :: Chromo -> G Chromo
mutate c = do
    locus <- randomGR (0, length c - 1)
    new <- randomG
    return $ take locus c ++ (new : drop (locus + 1) c)


main :: IO ()
main = do
    gen <- getStdGen
    let (_, n, _) = runG gen
    print n

