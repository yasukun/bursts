{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Lib
import System.Console.CmdArgs

data Bursts = Bursts {
      numargs :: [Int],
      numstate :: Int,
      numgamma :: Int
    } deriving (Show, Data, Typeable)

bursts = Bursts
         {
           numargs = def &= args &= typ "NUMS",
           numstate = 2 &= args &= typ "NUM",
           numgamma = 1 &= args &= typ "NUM"
         }

main = do
  opts <- cmdArgs bursts
  print $ kleinberg (numargs opts) $ defOpts {state=(numstate opts), gamma=(numgamma opts)}
