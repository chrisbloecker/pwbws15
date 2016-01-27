module Genetic
  where
--------------------------------------------------------------------------------
import           Control.Monad               ((>=>), replicateM, foldM)
import           Control.Monad.State         (State, state, get, put)
import           System.Random               (RandomGen, randomR, randomRs, next)
import           Data.Ord                    (comparing)
import           Data.Maybe                  (isJust)
import           Data.List                   (inits, tails)
import           Data.Sequence               (Seq, (><), (<|), empty, index, sortBy, fromList)
import           Constructomat               (Constructomat(..), Instruction, PlanId)
--------------------------------------------------------------------------------
import qualified Data.Sequence as S          (null, length, filter, take, drop, zip)
--------------------------------------------------------------------------------
type Breed = Individuum -> Maybe Constructomat
type Base  = PlanId
data Individuum = Individuum { getIndividuum :: ![Base] }           deriving (Show, Eq, Ord)
data Population = Population { getPopulation :: !(Seq Individuum) } deriving (Show)

instance Monoid Population where
  mempty                                  = Population empty
  mappend (Population p1) (Population p2) = Population (p1 >< p2)
--------------------------------------------------------------------------------

evolve :: (RandomGen g) => Base -> Breed -> State g [Base]
evolve maxBase breed = do
  initial      <- mkPopulation maxBase breed 100
  Population p <- foldM (\p _ -> theNextGeneration breed maxBase p) initial [1..35 :: Int]
  return . getIndividuum . flip index 0 $ p


theNextGeneration :: (RandomGen g) => Breed -> Base -> Population -> State g Population
theNextGeneration breed maxBase p@(Population oldPopulation) = do
  Population fresh   <- mkPopulation maxBase breed 100
  Population mutated <- mutate       maxBase p
  Population crossed <- crossover            p
  let pool = mconcat [mutated, crossed, oldPopulation, fresh]
      newGeneration = S.take 100 . fmap fst . sortBy (flip $ comparing snd) . S.filter (isJust . snd) . fmap (\i -> (i, value <$> breed i)) $ pool
  return . Population $ newGeneration


mkPopulation :: (RandomGen g) => Base -> Breed -> Int -> State g Population
mkPopulation maxBase breed n = do
  population <- fromList <$> replicateM n (mkIndividuum maxBase breed)
  return . Population $ fromList (map Individuum $ inits [0 .. maxBase]) >< fromList (map Individuum $ tails [0 .. maxBase]) >< population


mkIndividuum :: (RandomGen g) => Base -> Breed -> State g Individuum
mkIndividuum maxBase breed = do
  (n, gen') <- randomR (0, 10) <$> get
  put gen'
  let res = Individuum . take n . randomRs (0, maxBase) $ gen'
  case breed res of
    Nothing -> mkIndividuum maxBase breed
    Just _  -> return res


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
  population' <- mapM (uncurry crossoverPair) $ population `S.zip` shuffled
  return (Population population')


crossoverPair :: (RandomGen g) => Individuum -> Individuum -> State g Individuum
crossoverPair (Individuum i1) (Individuum i2) = state $ \gen ->
  let (n1, gen')  = randomR (0, length i1) gen
      (n2, gen'') = randomR (0, length i2) gen'
  in (Individuum $ take n1 i1 ++ drop n2 i2, gen'')


shuffle :: (RandomGen g) => Seq a -> State g (Seq a)
shuffle s | S.null s  = return s
          | otherwise = do
              (n,  gen') <- randomR (0, S.length s - 1) <$> get
              put gen'
              s' <- shuffle (S.take n s >< S.drop (n+1) s)
              return (s `index` n <| s')


breed :: Constructomat -> [Instruction] -> Breed
breed c is (Individuum i) = foldr ((>=>) . (is !!)) return i c
