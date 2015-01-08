module Strain where

keep ::  (t -> Bool) -> [t] -> [t]
keep _ [] = []
keep p [x] = [x | p x]
keep p (x:xs)
    | p x = x : keep p xs
    | otherwise = keep p xs

{- discard is simply the inverse of keep, so use not -}
discard ::  (t -> Bool) -> [t] -> [t]
discard p = keep (not . p)
