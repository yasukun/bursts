module Lib
    ( kleinberg
     ,KleinbergOpts(state,gamma)
     ,defOpts
     ,computeK
     ,computeOpt
    ) where

import Data.List
import Data.Matrix

calcGaps :: Num b => [b] -> [b]
calcGaps offsets =
    map (\(a,b) -> a - b) $ zip list1 list2
    where
      list1 = tail offsets
      list2 = init offsets

-- # Compute an upper bound on the number of states used in the optimal state
-- # sequence, as per Kleinberg.
computeK
  :: (Integral r, Integral b, Real a, Real a1, Foldable t) =>
     a1 -> t r -> a -> b
computeK state gaps total =
    ceiling $ 1 + c + b
    where
      a = 1 / (fromIntegral $ minimum gaps)
      b = logBase (realToFrac state) a
      c = logBase (realToFrac total) (realToFrac state)

data KleinbergOpts = KleinbergOpts {
      state :: Int,
      gamma :: Int
    } deriving Show

defOpts :: KleinbergOpts
defOpts =  KleinbergOpts {state=2, gamma=1}

computeOpt k tau f gaps cprime qprime coll =
    fst
    where
      fst = coll !! 0
      -- cost = map (\ell -> (realToFrac (cprime !! ell)) + tau ell fst) $ take k [1..]

kleinberg offsets opts =
    computeOpt k tau f gaps c q $ take count [1..]
    where offsets' = sort offsets
          gaps = calcGaps offsets'
          total = sum gaps
          count = length gaps
          ghat = (fromIntegral total) / (fromIntegral count)
          k = computeK (state opts) gaps total
          gammalogn = realToFrac (gamma opts) * log (fromIntegral count)
          tau = \x y -> if x >= y then 0 else (y -x) * gammalogn
          alpha = map (\x -> realToFrac (state opts) ** x / ghat) $ take (k - 1) [0..]
          f = \j x ->  (alpha !! j) * (exp $ -1 * alpha !! j * x)
          inf = 1/0
          na = 0/0
          c = 0:(take (k - 1) $ repeat inf)
          q = matrix k total $ \(i,j) -> na
