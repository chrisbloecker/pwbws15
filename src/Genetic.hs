{-# LANGUAGE RecordWildCards #-}

module Genetic
  where

--------------------------------------------------------------------------------
import Control.Monad
import Control.Monad.State
import System.Random
import Data.Maybe          (mapMaybe, fromJust)
import Data.List           (maximumBy, sortOn)
import Data.Function       (on)
--------------------------------------------------------------------------------
import Constructomat
--------------------------------------------------------------------------------
type Breed = Individuum -> Maybe Constructomat
type Individuum = [PlanId]

data Population = Population { population :: ![Individuum]
                             , maxBase    :: PlanId
                             }
  deriving (Show)
--------------------------------------------------------------------------------

evolve :: (RandomGen g) => PlanId -> Eval -> Breed -> State g Constructomat
evolve pid eval breed = do
  Population p _ <- mkPopulation pid >>= repeatM 30 (generation eval breed)
  return . fromJust . breed . head $ p


repeatM :: (Monad m) => Int -> (a -> m a) -> a -> m a
repeatM n f x | n == 0    = return x
              | n == 1    = f x
              | otherwise = g
  where g = f x >>= repeatM (n-1) f


generation :: (RandomGen g) => Eval -> Breed -> Population -> State g Population
generation eval breed p@(Population oldPopulation maxBase) = do
  Population mutated _ <- mutate p
  Population crossed _ <- crossover p
  let newGeneration = take 100 . reverse . map transitions . sortOn value . mapMaybe breed $ oldPopulation ++ mutated ++ crossed
  return $ Population newGeneration maxBase


mkPopulation :: (RandomGen g) => PlanId -> State g Population
mkPopulation pid = do
  population <- replicateM 100 (mkIndividuum pid)
  return . flip Population pid $ [] : population


mkIndividuum :: (RandomGen g) => PlanId -> State g Individuum
mkIndividuum pid = state $ \gen ->
  let (n, gen') = randomR (0, 10) gen
      res       = take n $ randomRs (0, pid) gen'
  in (res, gen')


mutate :: (RandomGen g) => Population -> State g Population
mutate Population{..} = do
  population' <- mapM (mutateIndividuum maxBase) population
  return (Population population' maxBase)


mutateIndividuum :: (RandomGen g) => PlanId -> Individuum -> State g Individuum
mutateIndividuum pid i = state $ \gen ->
  let ps = take (length i) $ randomRs (0.0, 1.0) gen :: [Double]
      ns = randomRs (0, pid) gen :: [PlanId]
      (_, gen') = next gen
  in (zipWith3 (\x n p -> if p > 0.9 then n else x) i ns ps, gen')


crossover :: (RandomGen g) => Population -> State g Population
crossover Population{..} = do
  shuffled <- shuffle population
  population' <- zipWithM crossoverPair population shuffled
  return (Population population' maxBase)


crossoverPair :: (RandomGen g) => Individuum -> Individuum -> State g Individuum
crossoverPair i1 i2 = state $ \gen ->
  let (n1, gen')  = randomR (0, length i1) gen
      (n2, gen'') = randomR (0, length i2) gen'
  in (take n1 i2 ++ drop n2 i2, gen'')


shuffle :: (RandomGen g) => [a] -> State g [a]
shuffle [] = return []
shuffle l  = do
  (n,  gen') <- randomR (0, length l - 1) <$> get
  put gen'
  l' <- shuffle (take n l ++ drop (n+1) l)
  return (l !! n:l')


breed :: Constructomat -> [Instruction] -> Breed
breed c is i = foldr ((>=>) . (is !!)) return i c
