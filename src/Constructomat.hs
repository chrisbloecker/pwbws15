{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
module Constructomat
  where
--------------------------------------------------------------------------------
import Data.Sequence (Seq, (|>))
--------------------------------------------------------------------------------
type Price    = Int
type Amount   = Int
type Plan     = ([Product], [Product], CoolingLiquid)
type PlanId   = Int
type Product  = Int
type CoolingLiquid = Amount

newtype Transition = Transition { unTransition :: Int }
data Constructomat = Constructomat { amounts     :: ![Amount]
                                   , value       :: !Price
                                   , transitions :: !(Seq PlanId)
                                   }
  deriving (Show, Ord)

instance Eq Constructomat where
  c1 == c2 = amounts c1 == amounts c2

type Instruction = Constructomat -> Maybe Constructomat
type Eval        = [Amount] -> Price
--------------------------------------------------------------------------------

eval :: [Price] -> Eval
eval = \ps -> sum . map penalty . zipWith (*) ps
  where
    penalty :: Price -> Price
    penalty x | x < 0     = -(x*x)
              | otherwise = x


mkInstruction :: Eval -> Int -> PlanId -> Plan -> Instruction
mkInstruction eval n pid (ins, outs, liquid) =
  let inAmounts  = counts n ins  ++ [liquid]
      outAmounts = counts n outs ++ [0]
      delta      = zipWith (-) outAmounts inAmounts
  in \Constructomat{..} -> if and (zipWith (>=) amounts inAmounts)
             then let amounts' = zipWith (+) amounts delta
                  in Just $ Constructomat amounts' (eval amounts') (transitions |> pid)
             else Nothing


counts :: Int -> [Product] -> [Amount]
counts n = foldr (zipWith (+) . singleton) zeros
  where
    singleton :: Product -> [Amount]
    singleton p = replicate p 0 ++ [1] ++ replicate (n-p-1) 0

    zeros :: [Amount]
    zeros = replicate n 0
