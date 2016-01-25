{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
module Search
  where
--------------------------------------------------------------------------------
import           Prelude                     hiding (iterate)
import           Data.Foldable                      (minimumBy)
import           Data.Ord                           (comparing)
import           Data.Maybe                         (mapMaybe)
import           Data.Set                           (Set, singleton, union, difference, fold, empty)
import           Constructomat
--------------------------------------------------------------------------------
import qualified Data.Set      as S
--------------------------------------------------------------------------------
data SearchMode = Complete
                | Depth Int
--------------------------------------------------------------------------------
data Exhaustive = Exhaustive { state        :: !(Set Constructomat)
                             , newStates    :: !(Set Constructomat)
                             , instructions :: ![Instruction]
                             }
--------------------------------------------------------------------------------

exhaustive :: Constructomat -> [Instruction] -> [PlanId]
exhaustive c is = let searchSpace = state . iterate $ Exhaustive (singleton c) (singleton c) is
                  in foldr (:) [] . transitions . minimumBy (flip $ comparing value) $ searchSpace
  where
    step :: Exhaustive -> Exhaustive
    step s@Exhaustive{..} = let state' = fold union empty . S.map (\st -> S.fromList $ mapMaybe ($st) instructions) $ newStates
                            in s { state     = state `union` state'
                                 , newStates = state' `difference` state
                                 }


    iterate :: Exhaustive -> Exhaustive
    iterate s = let s' = step s
                in if null (newStates s')
                  then s
                  else iterate s'
