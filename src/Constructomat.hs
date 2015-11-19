module Constructomat
  (module Constructomat)
  where

--------------------------------------------------------------------------------
import Model as Constructomat
--------------------------------------------------------------------------------

data Constructomat = Constructomat { amounts     :: !Amounts
                                   , transitions :: ![PlanId]
                                   }
  deriving (Show)

instance Eq Constructomat where
  c1 == c2 = amounts c1 == amounts c2

type Instruction = Constructomat -> Maybe Constructomat

--------------------------------------------------------------------------------

worth :: Prices -> (Constructomat -> Price)
worth = \ps -> sum . map penalty . zipWith (*) ps . amounts
  where
    penalty :: Price -> Price
    penalty x | x < 0     = -(x*x)
              | otherwise = x


mkInstruction :: Int -> PlanId -> Plan -> Instruction
mkInstruction n pid (ins, outs, liquid) =
  let inAmounts  = counts n ins  ++ [liquid]
      outAmounts = counts n outs ++ [0]
      delta     = zipWith (-) outAmounts inAmounts
  in \c -> if all id (zipWith (>=) (amounts c) inAmounts)
             then Just $ Constructomat (zipWith (+) (amounts c) delta) (pid:transitions c)
             else Nothing


counts :: Int -> Products -> Amounts
counts n ps = foldr (zipWith (+)) zeros (map singleton ps)
  where
    singleton :: Product -> Amounts
    singleton p = replicate p 0 ++ [1] ++ replicate (n-p-1) 0

    zeros :: Amounts
    zeros = replicate n 0
