module Tutorial where

maxL :: [Int] -> Int
maxL []  = 0
maxL [x] = x
maxL ls = max (maxL $ take sz ls) (maxL $ drop sz ls)
  where
    sz = length ls `div` 2
