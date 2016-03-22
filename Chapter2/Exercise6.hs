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
module Exercise6 where

import Control.Monad.State
import Control.Parallel.Strategies
import Data.Bits
import Data.Function
import Data.List
import System.Random

import Debug.Trace


data Term = A0 | A1 | D0 | D1 | D2 | D3 deriving (Show, Enum)
data Expr = Term Term | Not Expr
          | And Expr Expr | Or Expr Expr | If Expr Expr Expr
          deriving Show

eval :: Expr -> [Bool] -> Bool
eval (Term A0)  (a0:_)           = a0
eval (Term A1)  (_:a1:_)         = a1
eval (Term D0)  (_:_:d0:_)       = d0
eval (Term D1)  (_:_:_:d1:_)     = d1
eval (Term D2)  (_:_:_:_:d2:_)   = d2
eval (Term D3)  (_:_:_:_:_:d3:_) = d3
eval (Not e)    i                = not $ eval e i
eval (And a b)  i                = eval a i && eval b i
eval (Or a b)   i                = eval a i || eval b i
eval (If c t f) i               = if eval c i then eval t i else eval f i

replaceAt :: Int -> Expr -> Expr -> Expr
replaceAt n expr new = fst $ replaceAt' 0 expr
  where
    replaceAt' m t
      | m == n    = goDown m new
      | otherwise = goDown m t

    goDown m t@(Term _)  = (t, m + 1)
    goDown m t@(Not e)   =
        let (e', m') = replaceAt' (m + 1) e
        in  (Not e', m')
    goDown m e@(And a b) =
        let (a', m')  = replaceAt' (m + 1) a
            (b', m'') = replaceAt' m' b
        in  (And a' b', m'')
    goDown m e@(Or a b) =
        let (a', m')  = replaceAt' (m + 1) a
            (b', m'') = replaceAt' m' b
        in  (Or a' b', m'')
    goDown m e@(If c t f) =
        let (c', m')   = replaceAt' (m + 1) c
            (t', m'')  = replaceAt' m' t
            (f', m''') = replaceAt' m'' f
        in  (If c' t' f', m''')

number :: Expr -> ([(Expr, Int)], Int)
number = number' 0
  where
    number' n t@(Term _) = ([(t, n)], n + 1)
    number' n t@(Not e) =
        let (e', n') = number' (n + 1) e
        in  ((t, n):e', n')
    number' n e@(And a b) =
        let (a', n')  = number' (n + 1) a
            (b', n'') = number' n' b
        in  ((e, n):a'++b', n'')
    number' n e@(Or a b) =
        let (a', n')  = number' (n + 1) a
            (b', n'') = number' n' b
        in  ((e, n):a'++b', n'')
    number' n e@(If c t f) =
        let (c', n')   = number' (n + 1) c
            (t', n'')  = number' n' t
            (f', n''') = number' n'' f
        in  ((e, n):c'++t'++f', n''')

depth :: Expr -> Int
depth (Term _)    = 1
depth (Not e)     = 1 + depth e
depth (And a b)   = 1 + max (depth a) (depth b)
depth (Or a b)    = 1 + max (depth a) (depth b)
depth (If c t f)  = 1 + maximum [ depth c, depth t, depth f ]


type Fitness    = Int
type Chromo     = Expr
type Population = [Chromo]
data GOpts
  = GOpts
  { generator      :: StdGen
  , populationSize :: Int
  , crossoverRate  :: Double
  , eliteRate      :: Double
  } deriving Show
data GState
  = GState
  { options      :: GOpts
  , generation   :: Int
  , population   :: Population
  , elite        :: Population
  , fitnesses    :: [Fitness]
  , totalFitness :: Fitness
  , bestFitness  :: Fitness
  , bestInstance :: Chromo
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
          , elite        = mempty
          , fitnesses    = mempty
          , totalFitness = 0
          , bestFitness  = 0
          , bestInstance = undefined
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
            elite <- gets elite
            newInstances <- replicateM (count `div` 2) $ do
                parents <- selectParents
                (uncurry crossover) parents
            replace (elite ++ take (count - length elite) (concat newInstances))
            step
    step

initPopulation :: G ()
initPopulation = do
    count <- gets (populationSize . options)
    newInstances <- replicateM count (randomExpr 0)
    modify $ \s -> s { population = newInstances }

maxDepth :: Int
maxDepth = 5

randomExpr :: Int -> G Chromo
randomExpr d
  | d >= maxDepth = Term . toEnum <$> randomGR (0,5)
  | otherwise = do
      x <- randomGR (0,4 :: Int)
      case x of
        0 -> Term . toEnum <$> randomGR (0,5)
        1 -> Not <$> randomExpr (d + 1)
        2 -> And <$> randomExpr (d + 1) <*> randomExpr (d + 1)
        3 -> Or  <$> randomExpr (d + 1) <*> randomExpr (d + 1)
        4 -> If  <$> randomExpr (d + 1) <*> randomExpr (d + 1)
                                        <*> randomExpr (d + 1)

solved :: G Bool
solved = do
    found <- (>= top * 100 + 97 {- depth 3 -}) <$> gets bestFitness
    tooMuch <- (>= 100) <$> gets generation
    return $ found || tooMuch

calculateFitness :: G ()
calculateFitness = do
    count <- gets (populationSize . options)
    eliteRate <- gets (eliteRate . options)
    let eliteNum = floor (fromIntegral count * eliteRate)
    modify $ \s@GState{..} ->
      let values = map fitness population `using` parList rdeepseq
          accumulated = scanl (+) 0 values
          zipped = zip population values
          ordered = sortBy ((flip compare) `on` snd) zipped
          best = head ordered
          info = traceShow (fst best, snd $ number $ fst best, snd best)
      in  s { elite        = map fst $ take eliteNum ordered
            , fitnesses    = info accumulated
            , totalFitness = last accumulated
            , bestFitness  = snd best
            , bestInstance = fst best
            }

fitness :: Chromo -> Int
fitness expr = (100 - depth expr)
             + 100 * (length $ filter (\i -> eval expr i == reference i) allInputs)

top :: Int
top = 2 ^ 6

allInputs :: [[Bool]]
allInputs = map (\n -> map (testBit n) [0..5]) [0..top - 1]

reference :: [Bool] -> Bool
reference [a0,a1,d0,d1,d2,d3] = table a0 a1
  where
    table False False = d0
    table False True  = d1
    table True  False = d2
    table _     _     = d3

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
            let (aParts, aLen) = number a
                (bParts, bLen) = number b
            cutA <- randomGR (0, aLen - 1)
            cutB <- randomGR (0, bLen - 1)
            let o1 = replaceAt cutA a (map fst bParts !! cutB)
                o2 = replaceAt cutB b (map fst aParts !! cutA)
            return [ o1, o2 ]
        else return [a, b]

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
          , populationSize = 1000
          , crossoverRate  = 1
          , eliteRate      = 0.1
          }
        (_, n, _) = runG opts
    print n

