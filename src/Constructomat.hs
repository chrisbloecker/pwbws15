module Constructomat
  where

--------------------------------------------------------------------------------
import Model
--------------------------------------------------------------------------------

newtype Constructomat = Constructomat { unConstructomat :: Availabilities }
  deriving (Show)

type Instruction = Constructomat -> Maybe Constructomat

--------------------------------------------------------------------------------

worth :: Prices -> (Constructomat -> Price)
worth = \ps -> sum . zipWith (*) ps . unConstructomat


mkInstruction :: Int -> Plan -> Instruction
mkInstruction n (ins, outs, liquid) =
  let inAmounts  = counts n ins  ++ [liquid]
      outAmounts = counts n outs ++ [0]
      delta     = zipWith (-) outAmounts inAmounts
  in \c -> if all id (zipWith (>=) (unConstructomat c) inAmounts)
             then Just $ Constructomat (zipWith (+) (unConstructomat c) delta)
             else Nothing


counts :: Int -> Products -> Availabilities
counts n ps = foldr (zipWith (+)) zeros (map singleton ps)
  where
    singleton :: Product -> Availabilities
    singleton p = replicate p 0 ++ [1] ++ replicate (n-p-1) 0

    zeros :: Availabilities
    zeros = replicate n 0
