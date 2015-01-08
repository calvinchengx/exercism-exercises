module Bob (responseFor) where

import Data.Char

responseFor :: String -> String
responseFor xs
    | null xs || allFilteredCharacters xs == "" && last xs /= '?' && notNumber xs = "Fine. Be that way!"
    | allFilteredCharacters xs /= "" && shouting xs = "Whoa, chill out!"   
    | last xs == '?'    = "Sure."
    | otherwise         = "Whatever."

allFilteredCharacters ::  String -> String
allFilteredCharacters = Prelude.filter isLetter 
allFilteredCapsCharacters ::  String -> String
allFilteredCapsCharacters = Prelude.filter isUpper 
shouting ::  String -> Bool
shouting xs = allFilteredCharacters xs == allFilteredCapsCharacters xs
notNumber ::  String -> Bool
notNumber xs = Prelude.filter isNumber xs == ""


{-module Bob (responseFor) where-}

{-import Data.Char (isSpace, isLower, isAlpha)-}
{-import Data.List (isSuffixOf)-}

{-responseFor :: String -> String-}
{-responseFor phrase-}
  {-| isSilence = "Fine. Be that way!"-}
  {-| isYell = "Woah, chill out!"-}
  {-| isQuestion = "Sure."-}
  {-| otherwise = "Whatever."-}
  {-where isSilence = all isSpace phrase-}
        {-isYell = all (not . isLower) phrase && any isAlpha phrase-}
        {-isQuestion = "?" `isSuffixOf` phrase-}
