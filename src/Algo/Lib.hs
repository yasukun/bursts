module Algo.Lib
    ( kleinberg
     ,KleinbergOpts(..)
     ,defOpts
    ) where

import Data.List

data KleinbergOpts = KleinbergOpts {
      state :: Int,
      gamma :: Int
    } deriving Show

defOpts :: KleinbergOpts
defOpts =  KleinbergOpts {state=2, gamma=1}

minIndex :: Ord b => [b] -> Int
minIndex xs = head $ filter ((== minimum xs) . (xs !!)) [0..]

replaceNth :: (Eq a, Num a) => a -> a1 -> [a1] -> [a1]
replaceNth n newVal (x:xs)
    | n ==  0 = newVal:xs
    | otherwise = x:replaceNth (n-1) newVal xs

calcGaps :: Num b => [b] -> [b]
calcGaps offsets =
    map (\(a,b) -> a - b) $ zip list1 list2
    where
      list1 = tail offsets
      list2 = init offsets

log' :: Floating a => a -> a -> a
log' val base = log val / log base

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

computeOpt
  :: (Floating b, Fractional b1, Fractional b2, Fractional r1, Ord b,
      Real r) =>
     (Int -> r1 -> b)
     -> (b1 -> b2 -> b)
     -> Int
     -> [r]
     -> [b]
     -> [[Int]]
     -> [Int]
     -> ([b], [[Int]])
computeOpt _ _ _ _ caccum paccum [] = (caccum, paccum)
computeOpt f tau k gaps caccum paccum (t:ts) =
    computeOpt f tau k gaps cprime pprime ts
    where
      computeOpt' paths [] = []
      computeOpt' paths (j:js) =
          ret:computeOpt' path js
          where
            cost = map (\ell -> caccum !! ell + tau (realToFrac ell) (realToFrac j)
                       - log (f j $ realToFrac $ gaps  !!  t)) [0..(k - 1)]
            minidx = minIndex cost
            cprime' = cost !! minidx
            path = replaceNth j (slice 0 (t + 1) paths !! minidx ++ [j]) paths
            ret = (cprime',path)
      result = computeOpt' paccum [0..(k - 1)]
      pprime = snd $ last result
      cprime = map fst result

kleinberg :: Integral b => [b] -> KleinbergOpts -> [Int]
kleinberg offsets opts =
    paths !! (minIndex finalcost)
    where s = (state opts)
          g = (gamma opts)
          gaps = calcGaps $ sort offsets
          len = length gaps
          sumGaps = sum gaps
          k = ceiling $ 1 +
              log' (realToFrac sumGaps) (realToFrac s) +
              log' (1 / (realToFrac $ minimum gaps)) (realToFrac s)
          tau = let gammalogn = realToFrac g * log (realToFrac len)
                in \i j -> if i >= j then 0 else (j -i) * gammalogn
          f = let ghat = fromIntegral sumGaps / (fromIntegral len)
              in \i x -> let alpha = (realToFrac s) ** (realToFrac i) / ghat
                         in alpha * realToFrac (exp $ -alpha * x)
          inf = 1/0
          na = 0/0
          initC = 0:(take (k - 1) $ repeat inf)
          initPath = take k $ repeat [0]
          (finalcost, paths) = computeOpt f tau k gaps initC initPath [0..(len - 1)]
