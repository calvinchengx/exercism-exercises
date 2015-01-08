module Grains (square, total) where

{- Return a list of number of grains on each square given number of squares, n -}
square ::  (Num a, Integral b) => b -> a
square n = 2^(n-1)

{- Calculate total number of grains given fixed size chessboard with 64 squares -}
total ::  Integer
total = sum [ square n | n <- [1..64]]
