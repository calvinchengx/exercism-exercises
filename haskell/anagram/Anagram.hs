module Anagram where

import Data.List
import Data.Char

anagramsFor ::  String -> [String] -> [String]
anagramsFor _ [] = []
anagramsFor x xs
    | sortAndLower x == head [sortAndLower el | el <- xs ] && notItsOwnWord x (head xs) = head xs : anagramsFor x (tail xs)
    | otherwise = anagramsFor x (tail xs)

sortAndLower ::  String -> String
sortAndLower x = sort $ map toLower x

notItsOwnWord ::  String -> String -> Bool
notItsOwnWord a b = map toLower a /= map toLower b
