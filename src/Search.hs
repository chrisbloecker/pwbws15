{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
module Search
  where
--------------------------------------------------------------------------------
import           Prelude                     hiding (iterate)
import           Data.Foldable                      (minimumBy)
import           Data.Ord                           (comparing)
import           Data.Maybe                         (mapMaybe)
import           Data.List                          ((\\), nub)
--------------------------------------------------------------------------------
import           Constructomat
--------------------------------------------------------------------------------
data Exhaustive = Exhaustive { state        :: ![Constructomat]
                             , newStates    :: ![Constructomat]
                             , instructions :: ![Instruction]
                             }
--------------------------------------------------------------------------------

exhaustive :: Constructomat -> [Instruction] -> [PlanId]
exhaustive c is = let searchSpace = state . iterate $ Exhaustive [c] [c] is
                  in foldr (:) [] . transitions . minimumBy (flip $ comparing value) $ searchSpace
  where
    step :: Exhaustive -> Exhaustive
    step s@Exhaustive{..} = let state' = nub . concatMap (\st -> mapMaybe ($st) instructions) $ newStates
                            in s { state     = nub $ state ++ state'
                                 , newStates = state' \\ state
                                 }


    iterate :: Exhaustive -> Exhaustive
    iterate s = let s' = step s
                in if null (newStates s')
                  then s
                  else iterate s'
