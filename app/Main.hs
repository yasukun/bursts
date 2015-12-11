module Main where

import Lib

main :: IO ()
main = do
  print $ kleinberg [1,2,3,10,11,12,13,14,20,25,30] $ defOpts
