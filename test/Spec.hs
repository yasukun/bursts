import System.Exit (exitFailure, exitSuccess)
import Lib

main :: IO ()
main = do
  let result = kleinberg [10,20,30,40,100,101,102,103,104,105,200] $ defOpts
  if result == [0,0,0,0,0,0,3,3,3,3,3,0] then exitSuccess
  else exitFailure
