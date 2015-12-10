module Lib
    ( kleinberg
     ,KleinbergOpts(state,gamma)
     ,defOpts
     ,replaceNth
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

minIndex :: Ord b => [b] -> Int
minIndex xs = head $ filter ((== minimum xs) . (xs !!)) [0..]

replaceNth n newVal (x:xs)
    | n ==  0 = newVal:xs
    | otherwise = x:replaceNth (n-1) newVal xs

kleinberg offsets opts =
    foldl (\x t ->
               let (cprime, qprime) = x
               in foldl (\y j ->
                        let (c, q) = y
                            cost = map (\ell ->
                                            c !! ell + tau (realToFrac ell) (realToFrac j)) [0..(k - 1)]
                            minidx = minIndex cost
                            cresult = cost !! minidx - (f j (realToFrac (gaps !! t)))
                        in (replaceNth minidx cresult c, q))
               (cprime, qprime)
               [0..(k - 1)])
    (cdef, qdef)
    [0..(len - 1)]
    where gaps = calcGaps $ sort offsets
          total = sum gaps
          len = length gaps
          ghat = (fromIntegral total) / (fromIntegral len)
          k = computeK (state opts) gaps total
          gammalogn = realToFrac (gamma opts) * log (realToFrac len)
          tau = \x y -> if x >= y then 0 else (y -x) * gammalogn
          alpha = map (\x -> realToFrac (state opts) ** (realToFrac x) / ghat) [0..(k - 1)]
          f = \j x -> (alpha !! j) * (exp $ -1 * (alpha !! j) * x)
          inf = 1/0
          na = 0/0
          cdef = 0:(take (k - 1) $ repeat inf)
          qdef = matrix k total $ \(i,j) -> na
