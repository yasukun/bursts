module Lib
    ( kleinberg
     ,KleinbergOpts(state,gamma)
     ,defOpts
    ) where

import Data.List
import Data.Matrix

data KleinbergOpts = KleinbergOpts {
      state :: Int,
      gamma :: Int
    } deriving Show

defOpts :: KleinbergOpts
defOpts =  KleinbergOpts {state=2, gamma=1}

kleinberg offsets opts =
    foldl (\x t -> let (cprime,qprime) = x
                       c = map (\j ->
                                    let cost = map (\ell ->
                                                    cprime !! ell +
                                                    tau (realToFrac ell) (realToFrac j))
                                               [0..(k - 1)]
                                        minidx = minIndex cost
                                    in (cost !! minidx - log (f j $ realToFrac $ gaps !! t), minidx))
                           [0..(k - 1)]
                       elems = (map snd c)
                       q = foldl (\q' y ->
                                      let (ell, j) = y
                                          newval = realToFrac j
                                      in setElem  newval ((j - 1), (t - 1)) q'
                                 )
                           qprime
                           (zip elems [0..(length elems  - 1)])
                   in (map fst c,qprime))
              (initC, initQ)
              [0..(len - 1)]
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
          initQ = matrix k sumGaps $ \(i, j) -> na

minIndex :: Ord b => [b] -> Int
minIndex xs = head $ filter ((== minimum xs) . (xs !!)) [0..]

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
