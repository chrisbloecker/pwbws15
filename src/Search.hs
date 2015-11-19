{-# LANGUAGE RecordWildCards #-}

module Search
  where

--------------------------------------------------------------------------------
import           Prelude                     hiding (iterate)
import           Control.Parallel.Strategies        (parMap, rpar)
import           Data.Foldable                      (minimumBy)
import           Data.Ord                           (comparing)
import           Data.Maybe                         (catMaybes)
import           Data.List                          ((\\), nub)
--------------------------------------------------------------------------------
import           Constructomat
--------------------------------------------------------------------------------

data Search = Search { state        :: ![Constructomat]
                     , newStates    :: ![Constructomat]
                     , value        :: Constructomat -> Price
                     , instructions :: ![Instruction]
                     }

--------------------------------------------------------------------------------

mkSearch :: Constructomat -> (Constructomat -> Price) -> [Instruction] -> Search
mkSearch c p is = Search [c] [c] p is

step :: Search -> Search
step s@Search{..} = s { state     = nub $ state ++ state'
                      , newStates = state' \\ state
                      }
  where
    state' :: [Constructomat]
    state' = nub . catMaybes . concat $ parMap rpar (\st -> map ($st) instructions) newStates


iterate :: Search -> Search
iterate s = let s' = step s
            in if null (newStates s')
              then s
              else iterate s'


best :: Search -> Constructomat
best Search{..} = minimumBy (flip $ comparing value) state
