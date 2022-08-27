module Utils (lowerCaseFirst) where

import Data.Char (toLower)

lowerCaseFirst :: String -> String
lowerCaseFirst (x : xs) = [toLower x] <> xs
lowerCaseFirst xs = xs