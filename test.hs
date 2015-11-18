#!/usr/bin/env runhaskell
--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
module Test
  where
--------------------------------------------------------------------------------
import Turtle
--------------------------------------------------------------------------------

compile = do
  stdout "[INFO] Compiling constructomat..."
  stdout $ inproc "cabal" ["build"] ""
  stdout ""


runTest = do
  file <- find (suffix ".txt") "tests"
  stdout "[INFO] Running test"
  stdout . input $ file
  stdout $ inproc "./dist/build/pwbws15/pwbws15" [] (input file)
  stdout ""

--------------------------------------------------------------------------------

main :: IO ()
main = do
  sh compile
  sh runTest
