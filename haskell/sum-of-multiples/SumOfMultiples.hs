module SumOfMultiples (sumOfMultiplesDefault, sumOfMultiples) where

sumOfMultiplesDefault ::  Integral a => a -> a
sumOfMultiplesDefault n 
    | null [1..n] || null (filterM n) = 0
    | length [1..n] == 1 || length (filterM n) == 1 =  sum $ filter isMultipleOf3Or5 [1..n]
    | otherwise = sum $ filterM n

isMultipleOf3Or5 ::  Integral a => a -> Bool
isMultipleOf3Or5 x = x `mod` 3 == 0 || x `mod` 5 == 0 
filterM ::  Integral t => t -> [t]
filterM n = [x | x <- filter isMultipleOf3Or5 [1..n], x /= n]

sumOfMultiples ::  Integral a => [a] -> a -> a
sumOfMultiples xs n 
    | null [1..n] || null (filterM' xs n) = 0
    | length [1..n] == 1 || length (filterM' xs n) == 1 = sum $ filterM' xs n
    | otherwise = sum $ filterM' xs n

isMultiple' ::  Integral a => [a] -> a -> Bool
isMultiple' xs n = or [ n `mod` x == 0  | x <- xs ] 
filterM' ::  Integral t => [t] -> t -> [t]
filterM' xs n = [ x | x <- filter (isMultiple' xs) [1..n], x /= n ]
