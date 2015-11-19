module Main
  where

--------------------------------------------------------------------------------
import Prelude             hiding (iterate)
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

  let constructomat = Constructomat (amounts ++ [liquid]) []
      search = Search [constructomat] [] (worth prices) (map (uncurry $ mkInstruction (length amounts)) (zip [0..] plans))

  print . reverse . transitions . best . iterate $ search
