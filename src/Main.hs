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
  prices  <- (read :: String -> Prices)  <$> getLine
  amounts <- (read :: String -> Amounts) <$> getLine
  plans   <- (read :: String -> Plans)   <$> getLine
  liquid  <- (read :: String -> Amount)  <$> getLine

  let amounts'      = amounts ++ [liquid]
      eval          = worth prices
      constructomat = Constructomat amounts' (eval amounts') []
      search        = mkSearch constructomat (parMap rpar (uncurry $ mkInstruction eval (length amounts)) (zip [0..] plans))

  print . reverse . transitions . best . iterate $ search
