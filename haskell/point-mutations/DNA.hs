module DNA where

hammingDistance ::  (Ord b, Num a) => [b] -> [b] -> a
hammingDistance xs ys = sum $ zipWith oneOrZero xs ys
    
oneOrZero ::  (Ord a, Num a1) => a -> a -> a1
oneOrZero x y 
    | compare x y == EQ = 0
    | otherwise = 1
