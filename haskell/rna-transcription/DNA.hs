module DNA (toRNA) where

toRNA :: String -> String
toRNA dna = [ getComplement x | x <- dna ]

getComplement :: Char -> Char
getComplement x 
    | x == 'G' = 'C'
    | x == 'C' = 'G'
    | x == 'T' = 'A'
    | x == 'A' = 'U'
    | otherwise = error "Your dna strand is invalid"

{-A neater solution here-}
{-toRNA :: [Char] -> [Char]-}
{-toRNA = map transcribed-}
    {-where transcribed x = case x of-}
                            {-'G' -> 'C'-}
                            {-'C' -> 'G'-}
                            {-'A' -> 'U'-}
                            {-'T' -> 'A'-}
                            {-_ -> error "Unkown nucleobase"-}
