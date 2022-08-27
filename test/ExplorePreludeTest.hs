{-# OPTIONS_GHC -Wno-type-defaults #-}

module ExplorePreludeTest (spec) where

import Control.Exception (evaluate)
import ExplorePrelude (Animal (Cat, Fish), gotIntOption, gotPosIntOption, isBlank, isPalindrome, printSound, qsort, repeatStr, toMaybePositiveInt)

import Test.Hspec (Spec, anyException, describe, errorCall, it, shouldBe, shouldNotBe, shouldThrow)
import Text.Read (readMaybe)

import Text.RawString.QQ (r)

spec :: Spec
spec =
    describe "Standard Prelude" $ do
        describe "Misc" $ do
            it "divides floats" $ do
                5 / 2 `shouldBe` 2.5
            it "can sum of list of numbers" $ do
                [1 .. 5] `shouldBe` [1, 2, 3, 4, 5]
                sum [1 .. 5] `shouldBe` 15 -- 1 + 2 + 3 + 4 + 5 = 15
            it "can join lists of numbers" $ do
                [1, 2, 3] ++ [4, 5] `shouldBe` [1, 2, 3, 4, 5]
            it "can prepend to a list of numbers" $ do
                1 : [2, 3, 4, 5] `shouldBe` [1, 2, 3, 4, 5]
            it "can map over maybe" $ do
                {-
                    This is similar to this Elm code:

                        > Maybe.map ((+) 1) (Just 1)
                        Just 2 : Maybe number
                        > Maybe.map ((+) 1) Nothing
                        Nothing : Maybe number

                        > Just 1 |> Maybe.map ((+) 1)
                        Just 2 : Maybe number
                        > Nothing |> Maybe.map ((+) 1)
                        Nothing : Maybe number
                        >

                -}
                fmap (+ 1) (Just 1) `shouldBe` Just 2
                fmap (+ 1) Nothing `shouldBe` Nothing

        describe "Debugging" $ do
            it "uses `show` to inspect common data types" $ do
                show 42 `shouldBe` "42"
                show [1, 2] `shouldBe` "[1,2]"
                show ('a', "b", 3) `shouldBe` "('a',\"b\",3)"
                show ('a', "b", 3) `shouldBe` [r|('a',"b",3)|]

        describe "Working with lists" $ do
            it "selects the first element of a list (UNSAFE)" $ do
                head [1, 2, 3] `shouldBe` 1
                evaluate (head []) `shouldThrow` errorCall "Prelude.head: empty list"
            it "removes the first element of a list (UNSAFE)" $ do
                tail [1, 2, 3] `shouldBe` [2, 3]
                evaluate (tail []) `shouldThrow` errorCall "Prelude.tail: empty list"
            it "selects the nth element of a list (UNSAFE)" $ do
                [1, 2, 3] !! 1 `shouldBe` 2
                [1, 2, 3] !! 2 `shouldBe` 3
                evaluate ([1] !! 99) `shouldThrow` errorCall "Prelude.!!: index too large"
            it "selects the first n elements of a list (safe)" $ do
                take 3 [1, 2, 3, 4, 5] `shouldBe` [1, 2, 3]
                take 99 [1, 2] `shouldBe` [1, 2]
                take 99 ([] :: [Int]) `shouldBe` []

        describe "Working with strings" $ do
            describe "Test for empty or blank" $ do
                it "uses `null` to test emptiness" $ do
                    null "" `shouldBe` True
                    null " " `shouldNotBe` True
                    null "x" `shouldNotBe` True
                    null " x" `shouldNotBe` True
                    null "x " `shouldNotBe` True
                it "uses function composition to test blankness" $ do
                    isBlank "" `shouldBe` True
                    isBlank " " `shouldBe` True
                    isBlank "x" `shouldNotBe` True
                    isBlank " x" `shouldNotBe` True
                    isBlank "x " `shouldNotBe` True
            describe "String manipulation" $ do
                it "reverses" $
                    reverse "abc" `shouldBe` "cba"
                it "concats" $ do
                    "a" ++ "b" `shouldBe` "ab"
                    "a" ++ "b" ++ "c" `shouldBe` "abc"
                    concat ["a", "b", "c"] `shouldBe` "abc"
                    replicate 3 "!" `shouldBe` ["!", "!", "!"]
                    concat (replicate 3 "!") `shouldBe` "!!!"
                it "should be a palindrome" $ do
                    isPalindrome "noon" `shouldBe` True
                it "should not be a palindrome" $ do
                    isPalindrome "wat" `shouldNotBe` True
                it "repeats a string" $
                    repeatStr "!" 3 `shouldBe` "!!!"

        describe "Conversions" $ do
            it "converts an Int to String" $ do
                (show 1 :: String) `shouldBe` "1"
                (show (-1) :: String) `shouldBe` "-1"
                (show $ -1 :: String) `shouldBe` "-1"

            it "converts a String to an Int" $ do
                -- Don't use `read`!!
                evaluate (read "" :: Int) `shouldThrow` anyException
                readMaybe "1" `shouldBe` Just 1
                readMaybe "" `shouldBe` (Nothing :: Maybe Int)

            it "works with optional data" $ do
                gotIntOption (Just 1) `shouldBe` "YES[1]"
                gotIntOption (Just (-1)) `shouldBe` "YES[-1]"
                gotIntOption Nothing `shouldBe` "NO!"

            it "can force domain constraints via custom types" $ do
                gotPosIntOption (toMaybePositiveInt 1) `shouldBe` "YES[1]!"
                gotPosIntOption (toMaybePositiveInt (-1)) `shouldBe` "NO!!"
                gotPosIntOption Nothing `shouldBe` "NO!!"

        describe "Working with custom types" $ do
            it "uses pattern match" $
                printSound Cat `shouldBe` "Meow"
            it "can use `error` to mark WIP" $
                -- I should find a way to prevent calling these on production builds...
                -- This looks equivalent to Debug.todo in Elm
                evaluate (printSound Fish) `shouldThrow` anyException
        describe "Quick Sort" $ do
            it "sorts ints" $
                qsort [5, 3, 4, 2, 1] `shouldBe` [1, 2, 3, 4, 5]
            it "sorts floats" $
                qsort [5.5, 3.3, 4.4, 2.2, 1.1, 1.2] `shouldBe` [1.1, 1.2, 2.2, 3.3, 4.4, 5.5]
            it "sorts other types" $
                qsort [False, True, False, True] `shouldBe` [False, False, True, True]