module Main
  where
--------------------------------------------------------------------------------
import Prelude                     hiding (iterate)
import Control.Parallel.Strategies        (parMap, rpar)
import System.Random                      (mkStdGen)
import Control.Monad.State                (evalState)
import Constructomat
import Search
import Genetic
--------------------------------------------------------------------------------

main :: IO ()
main = do
  prices  <- (read :: String -> [Price])  <$> getLine
  amounts <- (read :: String -> [Amount]) <$> getLine
  plans   <- (read :: String -> [Plan])   <$> getLine
  liquid  <- (read :: String -> Amount)   <$> getLine

  let amounts'      = amounts ++ [liquid]
      eval'         = eval prices
      instructions  = parMap rpar (uncurry $ mkInstruction eval' (length amounts)) (zip [0..] plans)
      constructomat = Constructomat amounts' (eval' amounts') []
      search        = exhaustive constructomat instructions

      genetic = evalState (evolve (length plans - 1) (breed constructomat instructions)) (mkStdGen 42)

  --print . reverse . transitions $ search
  print genetic
