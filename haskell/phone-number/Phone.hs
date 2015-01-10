module Phone where

validNum ::  String
validNum = "01234567890"

number ::  String -> String
number n 
    | length (cleanUp n) == 11 && head (cleanUp n) == '1' = drop 1 (cleanUp n)
    | length (cleanUp n) == 10 = cleanUp n
    | otherwise = "0000000000"
    where
        cleanUp y = [ x | x <- y, x `elem` validNum]

areaCode :: String -> String 
areaCode n = take 3 (number n)

prettyPrint :: String -> String 
prettyPrint n = "(" ++ areaCode n ++ ") " ++ fst last7Num ++ "-" ++ snd last7Num
    where 
        last7Num = splitAt 3 (drop 3 (number n))
