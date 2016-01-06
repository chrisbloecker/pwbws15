{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Constructomat
  (module Constructomat)
  where

--------------------------------------------------------------------------------
import Model                       as Constructomat
import GHC.Generics                                 (Generic)
import Control.Parallel.Strategies                  (NFData)
--------------------------------------------------------------------------------

data Constructomat = Constructomat { amounts     :: !Amounts
                                   , value       :: !Price
                                   , transitions :: ![PlanId]
                                   }
  deriving (Show, Generic)

instance Eq Constructomat where
  c1 == c2 = amounts c1 == amounts c2

instance NFData Constructomat

type Instruction = Constructomat -> Maybe Constructomat
type Eval        = Amounts -> Price

--------------------------------------------------------------------------------

worth :: Prices -> (Amounts -> Price)
worth = \ps -> sum . map penalty . zipWith (*) ps
  where
    penalty :: Price -> Price
    penalty x | x < 0     = -(x*x)
              | otherwise = x


mkInstruction :: Eval -> Int -> PlanId -> Plan -> Instruction
mkInstruction eval n pid (ins, outs, liquid) =
  let inAmounts  = counts n ins  ++ [liquid]
      outAmounts = counts n outs ++ [0]
      delta     = zipWith (-) outAmounts inAmounts
  in \Constructomat{..} -> if all id (zipWith (>=) amounts inAmounts)
             then let amounts' = zipWith (+) amounts delta
                  in Just $ Constructomat amounts' (eval amounts') (pid:transitions)
             else Nothing


counts :: Int -> Products -> Amounts
counts n ps = foldr (zipWith (+)) zeros (map singleton ps)
  where
    singleton :: Product -> Amounts
    singleton p = replicate p 0 ++ [1] ++ replicate (n-p-1) 0

    zeros :: Amounts
    zeros = replicate n 0
