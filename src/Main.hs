module Main
  where

--------------------------------------------------------------------------------
import Prelude                     hiding (iterate)
import Control.Parallel.Strategies        (parMap, rpar)
--------------------------------------------------------------------------------
import Constructomat
import Search
--------------------------------------------------------------------------------

main :: IO ()
main = do
  prices  <- (read :: String -> [Price])  <$> getLine
  amounts <- (read :: String -> [Amount]) <$> getLine
  plans   <- (read :: String -> [Plan])   <$> getLine
  liquid  <- (read :: String -> Amount)   <$> getLine

  let amounts'      = amounts ++ [liquid]
      eval          = worth prices
      constructomat = Constructomat amounts' (eval amounts') []
      search        = exhaustive constructomat (parMap rpar (uncurry $ mkInstruction eval (length amounts)) (zip [0..] plans))

  print . reverse . transitions $ search
