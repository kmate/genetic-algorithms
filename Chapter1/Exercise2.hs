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
{-# LANGUAGE RecordWildCards #-}
module Exercise2 where

import Control.Monad.State
import System.Random

import Debug.Trace


type Fitness    = Int
type Chromo     = [Bool]
type Population = [Chromo]
data GOpts
  = GOpts
  { generator      :: StdGen
  , populationSize :: Int
  , crossoverRate  :: Double
  , mutationRate   :: Double
  } deriving Show
data GState
  = GState
  { options      :: GOpts
  , generation   :: Int
  , population   :: Population
  , fitnesses    :: [Fitness]
  , totalFitness :: Fitness
  , bestFitness  :: Fitness
  } deriving Show
type G = State GState

runG :: GOpts -> (Population, Int, StdGen)
runG opts = (population finish, generation finish, generator $ options finish)
  where
    finish = execState evolve start
    start = GState
          { options      = opts
          , generation   = 0
          , population   = mempty
          , fitnesses    = mempty
          , totalFitness = 0
          , bestFitness  = 0
          }

randomG :: Random a => G a
randomG = state $ \s ->
    let (x, g') = random (generator $ options s)
    in  (x, s { options = (options s) { generator = g' } })

randomGR :: Random a => (a, a) -> G a
randomGR range = state $ \s ->
    let (x, g') = randomR range (generator $ options s)
    in  (x, s { options = (options s) { generator = g' } })

evolve :: G ()
evolve = do
    initPopulation
    let step = do
        done <- solved
        unless done $ do
            calculateFitness
            count <- gets (populationSize . options)
            newInstances <- replicateM (count `div` 2) $ do
                parents <- selectParents
                offsprings <- (uncurry crossover) parents
                mapM mutate offsprings
            replace (take count $ concat newInstances)
            
            total <- fromIntegral <$> gets totalFitness
            let average = total / fromIntegral count
            best <- gets bestFitness
            trace (show best ++ " " ++ show average) step
    step

initPopulation :: G ()
initPopulation = do
    count <- gets (populationSize . options)
    newInstances <- replicateM count $ do
        let chromoSize = 20
        replicateM chromoSize randomG
    modify $ \s -> s { population = newInstances }

solved :: G Bool
solved = (>= 100) <$> gets generation

calculateFitness :: G ()
calculateFitness = modify $ \s@GState{..} ->
    let values = map fitness population
        accumulated = scanl (+) 0 values
    in  s { fitnesses    = accumulated
          , totalFitness = last accumulated
          , bestFitness  = maximum values
          }

fitness :: Chromo -> Int
fitness = fromBinary

fromBinary :: [Bool] -> Int
fromBinary = go 0
  where
    go n [] = n
    go n (d:ds) = go (2 * n + if d then 1 else 0) ds

selectParents :: G (Chromo, Chromo)
selectParents = do
    a <- roulette
    b <- roulette
    return (a, b)

roulette :: G Chromo
roulette = do
    total <- gets totalFitness
    ball <- randomGR (0, total - 1)
    fitnesses <- gets fitnesses
    let index = length (takeWhile (<= ball) fitnesses) - 1
    (!! index) <$> gets population

crossover :: Chromo -> Chromo -> G [Chromo]
crossover a b = do
    dice <- randomG
    threshold <- gets (crossoverRate . options)
    if dice <= threshold
        then do
            locus <- randomGR (0, length a - 1)
            return [ take locus a ++ drop locus b
                   , take locus b ++ drop locus a ]
        else return [a, b]

mutate :: Chromo -> G Chromo
mutate = mapM $ \c -> do
    dice <- randomG
    threshold <- gets (mutationRate . options)
    if dice <= threshold
        then return $ not c
        else return c

replace :: Population -> G ()
replace newInstances = modify $ \s@GState{..} -> s
    { generation   = generation + 1
    , population   = newInstances
    , fitnesses    = mempty
    }


main :: IO ()
main = do
    gen <- getStdGen
    let opts
          = GOpts
          { generator      = gen
          , populationSize = 100
          , crossoverRate  = 0.7
          , mutationRate   = 0.001
          }
        (_, n, _) = runG opts
    print n

