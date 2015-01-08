module Sublist 
(
    Sublist(Equal, Sublist, Superlist, Unequal), 
    sublist
)
where

import Data.List (tails)
data Sublist = Equal | Sublist | Superlist | Unequal deriving (Show, Eq)

{- Function that determines if a list is a sublist of another list -}
{- This is the equivalent of Data.List's isInfixOf -}
search :: (Eq a) => [a] -> [a] -> Bool  
search needle haystack =   
    let nlen = length needle  
    in foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)  

sublist :: Eq a => [a] -> [a] -> Sublist
sublist x y 
    | search x y && search y x = Equal
    | search x y    = Sublist
    | search y x    = Superlist
    | otherwise     = Unequal
