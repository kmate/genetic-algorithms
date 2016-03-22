{-
Implement a genetic programming algorithm and use it to solve the "6−multiplexer"
problem (Koza 1992). In this problem there are six Boolean−valued terminals,
{a0, a1, d0,d1, d2, d3}, and four functions, {AND, OR, NOT, IF}. The first three
functions are the usual logical operators, taking two, two, and one argument
respectively, and the IF function takes three arguments. (IF X Y Z) evaluates its
first argument X. If X is true, the second argument Y is evaluated; otherwise the
third argument Z is evaluated. The problem is to find a program that will return
the value of the d terminal that is addressed by the two a terminals. E.g., if
a0 = 0 and a1 = 1, the address is 01 and the answer is the value of d1. Likewise,
if a0 = 1 and a1 = 1, the address is 11 and the answer is the value of d3.
Experiment with different initial conditions, crossover rates, and population
sizes. (Start with a population size of 300.) The fitness of a program should be
the fraction of correct answers over all 2^6 possible fitness cases (i.e., values
of the six terminals).
-}
{-# LANGUAGE RecordWildCards #-}
module Exercise1 where

import Control.Monad.State
import System.Random


data Term = A0 | A1 | D0 | D1 | D2 | D3 deriving (Show, Bounded)
data Expr = Term Term | Not Expr
          | And Expr Expr | Or Expr Expr | If Expr Expr Expr
          deriving Show

type Fitness    = Int
type Chromo     = Expr
type Population = [Chromo]
data GOpts
  = GOpts
  { generator      :: StdGen
  , populationSize :: Int
  , crossoverRate  :: Double
  } deriving Show
data GState
  = GState
  { options      :: GOpts
  , generation   :: Int
  , population   :: Population
  , fitnesses    :: [Fitness]
  , totalFitness :: Fitness
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
                (uncurry crossover) parents
            replace (take count $ concat newInstances)
            step
    step

initPopulation :: G ()
initPopulation = do
    count <- gets (populationSize . options)
    newInstances <- replicateM count $ error "TODO: implement initPopulation"
    modify $ \s -> s { population = newInstances }

solved :: G Bool
solved = error "TODO: implement solved"

calculateFitness :: G ()
calculateFitness = modify $ \s@GState{..} ->
    let values = map fitness population
        accumulated = scanl (+) 0 values
    in  s { fitnesses    = accumulated
          , totalFitness = last accumulated
          }

fitness :: Chromo -> Int
fitness = error "TODO: implement fitness"

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
crossover a b = error "TODO: implement crossover"

replace :: Population -> G ()
replace newInstances = modify $ \s@GState{..} -> s
    { generation   = generation + 1
    , population   = newInstances
    , fitnesses    = mempty
    , totalFitness = 0
    }


main :: IO ()
main = do
    gen <- getStdGen
    let opts
          = GOpts
          { generator      = gen
          , populationSize = 300
          , crossoverRate  = 0.7
          }
        (_, n, _) = runG opts
    print n

