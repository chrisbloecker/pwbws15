{-# LANGUAGE RecordWildCards #-}

module Search
  where

--------------------------------------------------------------------------------
import           Prelude                     hiding (iterate)
import           Control.Parallel.Strategies        (parMap, rdeepseq)
import           Data.Foldable                      (minimumBy)
import           Data.Ord                           (comparing)
import           Data.Maybe                         (catMaybes)
import           Data.List                          ((\\), nub)
--------------------------------------------------------------------------------
import           Constructomat
--------------------------------------------------------------------------------

data Exhaustive = Exhaustive { state        :: ![Constructomat]
                             , newStates    :: ![Constructomat]
                             , instructions :: ![Instruction]
                             }

--------------------------------------------------------------------------------

exhaustive :: Constructomat -> [Instruction] -> Constructomat
exhaustive c is = let searchSpace = state . iterate $ Exhaustive [c] [c] is
                  in minimumBy (flip $ comparing value) searchSpace
  where
    step :: Exhaustive -> Exhaustive
    step s@Exhaustive{..} = let state' = nub . catMaybes . concat $ parMap rdeepseq (\st -> map ($st) instructions) newStates
                            in s { state     = nub $ state ++ state'
                                 , newStates = state' \\ state
                                 }


    iterate :: Exhaustive -> Exhaustive
    iterate s = let s' = step s
                in if null (newStates s')
                  then s
                  else iterate s'

--------------------------------------------------------------------------------
