module ExplorePrelude (
    isBlank,
    isPalindrome,
    repeatStr,
    gotIntOption,
    gotPosIntOption,
    toMaybePositiveInt,
    printSound,
    Animal (..),
) where

import Data.Char (isSpace)

isBlank :: String -> Bool
isBlank = null . dropWhile isSpace -- NOTE: `dropWhile isSpace` only trims left

isPalindrome :: String -> Bool
isPalindrome str = str == reverse str

repeatStr :: String -> Int -> String
repeatStr str n = concat $ replicate n str

newtype PositiveInt = PositiveInt {unPositiveInt :: Int}

toMaybePositiveInt :: Int -> Maybe PositiveInt
toMaybePositiveInt n =
    if n <= 0
        then Nothing
        else Just (PositiveInt n)

getPositiveInt :: PositiveInt -> Int
getPositiveInt = unPositiveInt

gotIntOption :: Maybe Int -> String
gotIntOption maybeInt = case maybeInt of
    Just n -> "YES[" ++ show n ++ "]"
    Nothing -> "NO!"

gotPosIntOption :: Maybe PositiveInt -> String
gotPosIntOption maybeInt = case maybeInt of
    Just n -> gotIntOption (Just $ getPositiveInt n) ++ "!"
    Nothing -> gotIntOption Nothing ++ "!"

data Animal = Cat | Dog | Fish

printSound :: Animal -> String
printSound animal =
    case animal of
        Cat -> "Meow"
        Dog -> "Woof"
        _ ->
            error "handle Fish later..."