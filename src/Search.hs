{-# LANGUAGE RecordWildCards #-}

module Search
  where

--------------------------------------------------------------------------------
import           Prelude    hiding (iterate)
import           Data.Maybe        (catMaybes)
import           Data.List         (nub)
--------------------------------------------------------------------------------
import           Constructomat
--------------------------------------------------------------------------------

data Search = Search { state        :: [Constructomat]
                     , value        :: Constructomat -> Price
                     , instructions :: [Instruction]
                     }

--------------------------------------------------------------------------------

step :: Search -> Search
step s@Search{..} = s { state = nub $ state ++ state' }
  where
    state' :: [Constructomat]
    state' = catMaybes $ state >>= \st -> map ($st) instructions


iterate :: Search -> Search
iterate s = let s' = step s
            in if state s == state s'
              then s
              else iterate s'


best :: Search -> Constructomat
best s@Search{..} = case state of
                      []         -> error "[ERROR] This can't happen. We always have at least one configuration, i.e. the initial configuration."
                      (c:[])     -> c
                      (c1:c2:cs) -> let s' = if value c1 >= value c2
                                               then s { state = c1:cs }
                                               else s { state = c2:cs }
                                    in best s'
