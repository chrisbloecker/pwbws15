{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
module Genetic
  where
--------------------------------------------------------------------------------
import Control.Monad
import Control.Monad.State
import System.Random
import Data.Maybe            (mapMaybe, fromJust)
import Data.List             (sortOn)
import Data.Function.Memoize (deriveMemoizable)
--------------------------------------------------------------------------------
import Constructomat
--------------------------------------------------------------------------------
type Breed = Individuum -> Maybe Constructomat
type Base  = PlanId
data Individuum = Individuum { getIndividuum :: ![Base] }       deriving (Show)
data Population = Population { getPopulation :: ![Individuum] } deriving (Show)
--------------------------------------------------------------------------------
deriveMemoizable ''Individuum
--------------------------------------------------------------------------------

evolve :: (RandomGen g) => Base -> Breed -> State g [Base]
evolve maxBase breed = do
  Population p <- mkPopulation maxBase >>= repeatM 30 (theNextGeneration breed maxBase)
  return . transitions . fromJust . breed . head $ p


repeatM :: (Monad m) => Int -> (a -> m a) -> a -> m a
repeatM n f x | n == 0    = return x
              | n == 1    = f x
              | otherwise = g
  where g = f x >>= repeatM (n-1) f


theNextGeneration :: (RandomGen g) => Breed -> Base -> Population -> State g Population
theNextGeneration breed maxBase p@(Population oldPopulation) = do
  Population fresh   <- mkPopulation maxBase
  Population mutated <- mutate       maxBase p
  Population crossed <- crossover            p
  --Population copied  _ <- copy      p
  let newGeneration = take 100 . reverse . map (Individuum . transitions) . sortOn value . mapMaybe breed $ fresh ++ mutated ++ crossed ++ oldPopulation
  return . Population $ newGeneration


mkPopulation :: (RandomGen g) => Base -> State g Population
mkPopulation maxBase = do
  population <- replicateM 100 (mkIndividuum maxBase)
  return . Population $ Individuum [] : population


mkIndividuum :: (RandomGen g) => Base -> State g Individuum
mkIndividuum maxBase = state $ \gen ->
  let (n, gen') = randomR (0, 10) gen
      res       = take n $ randomRs (0, maxBase) gen'
  in (Individuum res, gen')


mutate :: (RandomGen g) => Base -> Population -> State g Population
mutate maxBase (Population population) = do
  population' <- mapM (mutateIndividuum maxBase) population
  return (Population population')


mutateIndividuum :: (RandomGen g) => Base -> Individuum -> State g Individuum
mutateIndividuum maxBase (Individuum i) = state $ \gen ->
  let ps = take (length i) $ randomRs (0.0, 1.0) gen :: [Double]
      ns = randomRs (0, maxBase) gen :: [Base]
      (_, gen') = next gen
  in (Individuum $ zipWith3 (\x n p -> if p > 0.85 then n else x) i ns ps, gen')


crossover :: (RandomGen g) => Population -> State g Population
crossover (Population population) = do
  shuffled    <- shuffle population
  population' <- zipWithM crossoverPair population shuffled
  return (Population population')


crossoverPair :: (RandomGen g) => Individuum -> Individuum -> State g Individuum
crossoverPair (Individuum i1) (Individuum i2) = state $ \gen ->
  let (n1, gen')  = randomR (0, length i1) gen
      (n2, gen'') = randomR (0, length i2) gen'
  in (Individuum $ take n1 i1 ++ drop n2 i2, gen'')


shuffle :: (RandomGen g) => [a] -> State g [a]
shuffle [] = return []
shuffle l  = do
  (n,  gen') <- randomR (0, length l - 1) <$> get
  put gen'
  l' <- shuffle (take n l ++ drop (n+1) l)
  return (l !! n:l')


copy :: (RandomGen g) => Population -> State g Population
copy (Population population) = do
  population' <- mapM copyIndividuum population
  return (Population population')


copyIndividuum :: (RandomGen g) => Individuum -> State g Individuum
copyIndividuum (Individuum i) = state $ \gen ->
  let (n1, gen')  = randomR (0, length i) gen
      (n2, gen'') = randomR (0, length i) gen'
  in (Individuum $ take n1 i ++ drop n2 i, gen'')


breed :: Constructomat -> [Instruction] -> Breed
breed c is (Individuum i) = foldr ((>=>) . (is !!)) return i c
