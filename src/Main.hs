module Main
  where
--------------------------------------------------------------------------------
import Prelude                     hiding (iterate)
import Control.Parallel.Strategies        (parMap, rpar)
import Control.Concurrent                 (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Monad.State                (evalState)
import Data.Function.Memoize              (memoize)
import System.Random                      (mkStdGen)
import System.IO                          (hFlush, stdout)
import Constructomat                      (Constructomat (..), Price, Amount, Plan, eval, mkInstruction)
import Search                             (exhaustive)
import Genetic                            (evolve, breed)
--------------------------------------------------------------------------------

main :: IO ()
main = do
  -- parse input
  prices  <- (read :: String -> [Price])  <$> getLine
  amounts <- (read :: String -> [Amount]) <$> getLine
  plans   <- (read :: String -> [Plan])   <$> getLine
  liquid  <- (read :: String -> Amount)   <$> getLine

  let amounts'      = amounts ++ [liquid]
      eval'         = eval prices
      instructions  = parMap rpar (uncurry $ mkInstruction eval' (length amounts)) (zip [0..] plans)
      constructomat = Constructomat amounts' (eval' amounts') []
      search        = exhaustive constructomat instructions
      genetic       = evalState (evolve (length plans - 1) (breed constructomat instructions)) (mkStdGen 42)

  result <- newEmptyMVar

  -- run exhaustive search and genetic algorithm in parallel
  _ <- forkIO $ search  `seq` putMVar result search  -- >> putStrLn "Search finished"
  _ <- forkIO $ genetic `seq` putMVar result genetic -- >> putStrLn "Genetic finished"

  -- and output the solution from whichever finished first
  print . reverse =<< takeMVar result
  hFlush stdout
