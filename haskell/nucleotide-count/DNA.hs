{-# LANGUAGE TupleSections #-}
module DNA where

import Data.Map.Strict ( fromListWith, Map )
import Data.Char

dna :: String 
dna = "ACGT"

count :: Char -> String -> Int
count nuc strand
    | nuc `elem` dna = length $ Prelude.filter (==nuc) (Prelude.map toUpper strand)
    | otherwise = error $ "invalid nucleotide " ++ show nuc

nucleotideCounts ::  Num a => String -> Map Char a
nucleotideCounts xs = Data.Map.Strict.fromListWith (+) . (zeros ++) . Prelude.map (,1) $ Prelude.map toUpper xs
    where zeros = Prelude.map (,0) dna
