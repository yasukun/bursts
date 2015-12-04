module Lib
    ( someFunc
    ) where

import Data.List

someFunc :: IO ()
someFunc = putStrLn "someFunc"

kleinberg :: Ord a => [a] -> [a]
kleinberg offsets  =
    let offsets' = sort offsets
    in
      offsets'
