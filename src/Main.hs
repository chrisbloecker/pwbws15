module Main
  where

--------------------------------------------------------------------------------
import Control.Applicative ((<$>))
--------------------------------------------------------------------------------
import Constructomat
import Model
--------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "[INFO ] Running the awesomely awesome Constructomat!"

  prices       <- (read :: String -> Prices)         <$> getLine
  availability <- (read :: String -> Availabilities) <$> getLine
  plans        <- (read :: String -> Plans)          <$> getLine
  liquid       <- (read :: String -> Availability)   <$> getLine

  print (worth prices (Constructomat (availability ++ [liquid])))
