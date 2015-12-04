module Main where

import Lib

main :: IO ()
main = do
  print $ kleinberg [1,3,4,5,19,29] $ defOpts
