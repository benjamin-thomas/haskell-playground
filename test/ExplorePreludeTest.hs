{-# OPTIONS_GHC -Wno-type-defaults #-}

module ExplorePreludeTest (spec) where

import Control.Exception (evaluate)
import ExplorePrelude (Animal (Cat, Fish), gotIntOption, gotPosIntOption, isBlank, isPalindrome, printSound, qsort, repeatStr, toMaybePositiveInt)

import Test.Hspec (Example (Arg), Expectation, Spec, SpecWith, anyException, describe, errorCall, it, shouldBe, shouldNotBe, shouldThrow)
import Text.Read (readMaybe)

import Data.Functor ((<&>))
import Data.Maybe (catMaybes, fromMaybe, mapMaybe, maybeToList)
import Text.RawString.QQ (r)

test :: String -> Expectation -> SpecWith (Arg Expectation)
test = it

spec :: Spec
spec =
    describe "Standard Prelude" $ do
        describe "Misc" $ do
            it "learns about fmap and <$> and <&>" $ do
                fmap (+ 1) Nothing `shouldBe` Nothing
                fmap (+ 1) (Just 1) `shouldBe` Just 2
                (+ 1) `fmap` Nothing `shouldBe` Nothing
                (+ 1) <$> Nothing `shouldBe` Nothing
                (+ 1) `fmap` Just 1 `shouldBe` Just 2
                (+ 1) <$> Just 1 `shouldBe` Just 2
            test "<&> is the flipped version of <$>" $ do
                (Just 1 <&> (+ 1)) `shouldBe` (+ 1) <$> Just 1
                (Just 1 <&> (+ 1) <&> (* 2)) `shouldBe` Just 4 -- 1 |> +2 |> *2
            it "can do interesting stuff with `id`" $ do
                {- The commented functions generate hlint suggestions -}
                -- filter (\x -> x) [True, False] `shouldBe` [True]
                -- filter (\x -> not x) [True, False] `shouldBe` [False]
                filter id [True, False] `shouldBe` [True]
                filter not [True, False] `shouldBe` [False]
                -- id True `shouldBe` True
                -- not True `shouldBe` False
                -- id 3 `shouldBe` 3
                {- Like Elm's Maybe.withDefault! -}
                fromMaybe 0 Nothing `shouldBe` 0
                fromMaybe 0 (Just 1) `shouldBe` 1
                {-
                    The maybe function takes a default value, a function, and a Maybe value.
                    If the Maybe value is Nothing , the function returns the default value.
                    Otherwise, it applies the function to the value inside the Just and returns the result.

                    Prelude> maybe (-1) (\x -> x + 1) $ Nothing
                    -1
                    Prelude> maybe (-1) (\x -> x + 1) $ Just 1
                    2
                -}
                maybe (-1) (+ 1) Nothing `shouldBe` -1
                maybe (-1) (+ 1) (Just 1) `shouldBe` 2
            -- let fromMaybe2 default_ = maybe default_ id
            -- fromMaybe2 0 Nothing `shouldBe` 0
            -- fromMaybe2 0 (Just 1) `shouldBe` 1
            it "can implement factorial easily" $ do
                let fact n = product [1 .. n]
                fact 10 `shouldBe` 1 * 2 * 3 * 4 * 5 * 6 * 7 * 8 * 9 * 10
            it "can implement an average func" $ do
                let avg ns = case ns of
                        [] -> 0
                        _ -> sum ns `div` length ns
                avg [1 .. 3] `shouldBe` 2
                avg [] `shouldBe` 0
            describe "Another look at function composition" $ do
                it "applies from the right" $ do
                    ((+ 2) . (* 3) $ 4) `shouldBe` (4 * 3) + 2
                    ((* 3) . (+ 2) $ 4) `shouldBe` (4 + 2) * 3
            it "can make function calls more readable with infix syntax (backticks)" $ do
                let dblAndAdd x y = (* 2) x + y
                dblAndAdd 2 1 `shouldBe` 2 * 2 + 1
                2 `dblAndAdd` 1 `shouldBe` 2 * 2 + 1
            it "divides floats" $ do
                5 / 2 `shouldBe` 2.5
            it "can sum of list of numbers" $ do
                [1 .. 5] `shouldBe` [1, 2, 3, 4, 5]
                sum [1 .. 5] `shouldBe` 15 -- 1 + 2 + 3 + 4 + 5 = 15
                -- ===============================================================================
                -- All impls below do work, but I'm commenting them out here since I don't want to
                -- silence Hlint warnings globally (code can be simplified).
                -- ===============================================================================
                -- let sum' = foldl (\acc curr -> acc + curr) 0
                -- let sum' = foldl (+) 0
                -- sum' [1 .. 5] `shouldBe` 15 -- 1 + 2 + 3 + 4 + 5 = 15
                -- foldl (+) 0 [1 .. 5] `shouldBe` 15
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

                    NOTE:

                        fmap (\x -> x + 1) SAME AS ( +1)

                -}
                fmap (+ 1) (Just 1) `shouldBe` Just 2
                fmap (+ 1) Nothing `shouldBe` Nothing

            describe "concatMap" $ do
                it "works like this" $ do
                    {-
                    From the official doc:

                        Map a function over all the elements of a container and concatenate the resulting lists

                    I found this description elsewhere much clearer:
                        http://zvon.org/other/haskell/Outputprelude/concatMap_f.html

                        Creates a list from a list generating function by application of this function on all elements in a list passed as the second argument

                        Prelude> concatMap (\x -> [x + 1]) [1,2,3]
                        [2,3,4]
                        Prelude> map (\x -> x + 1) [1,2,3]
                        [2,3,4]

                        concatMap (\x -> maybeToList x) [Just 1, Just 2, Just 3]
                        SAME AS
                        concatMap maybeToList [Just 1, Just 2, Just 3]

                        concatMap maybeToList [Just 1, Just 2, Just 3]
                        SAME AS
                        catMaybes [Just 1, Just 2, Just 3]

                        Prelude Data.Maybe> fmap (+1) $ concatMap (\x -> maybeToList x) [Just 1, Just 2, Just 3]
                        [2,3,4]

                        Prelude Data.Maybe> concatMap ((fmap (+1)) . maybeToList) [Just 1, Just 2, Nothing]
                        [2,3]

                        concatMap (maybeToList . fmap (+ 1)) [Just 1, Just 2, Just 3]
                        SAME AS
                        mapMaybe (fmap (+ 1)) [Just 1, Just 2, Just 3]

                        fmap (+ (1 :: Int)) $ catMaybes [Just 1, Just 2, Just 3]
                        SAME AS
                        (+ (1)) <$> catMaybes [Just 1, Just 2, Just 3]
                        SAME AS
                        fmap (+ 1) (catMaybes [Just 1, Just 2, Just 3])
                    -}

                    concatMap (\x -> [x + 1]) [1, 2, 3] `shouldBe` [2, 3, 4]
                it "can filter elements with maybeToList" $ do
                    maybeToList (Just 1) `shouldBe` [1]
                    maybeToList (Nothing :: Maybe Int) `shouldBe` []
                    catMaybes [Just 1, Just 2, Just 3] `shouldBe` [1, 2, 3]
                    mapMaybe (fmap (+ 0)) [Just 1, Just 2, Just 3] `shouldBe` [1, 2, 3]
                    mapMaybe (fmap (+ 1)) [Just 1, Just 2, Just 3] `shouldBe` [2, 3, 4]
                    mapMaybe (fmap (+ 1)) [Just 1, Nothing, Just 3] `shouldBe` [2, 4]
                    catMaybes [Just 1, Just 2, Just 3] `shouldBe` [1, 2, 3]
                    fmap (+ 1) (catMaybes [Just 1, Just 2, Just 3]) `shouldBe` [2, 3, 4]
                    ((+ 1) <$> catMaybes [Just 1, Just 2, Just 3]) `shouldBe` [2, 3, 4]
            describe "<$>" $ do
                it "is like a \"reversed\" fmap" $ do
                    fmap (+ 1) [1, 2, 3] `shouldBe` [2, 3, 4]
                    (+ 1) <$> [1, 2, 3] `shouldBe` [2, 3, 4]

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

            it "selects the last elements of a list with `drop` (SAFE)" $ do
                drop 3 [1, 2, 3, 4, 5] `shouldBe` [4, 5]
                drop 99 [1, 2, 3, 4, 5] `shouldBe` []
            it "selects the nth element of a list (UNSAFE)" $ do
                [1, 2, 3] !! 1 `shouldBe` 2
                [1, 2, 3] !! 2 `shouldBe` 3
                evaluate ([1] !! 99) `shouldThrow` errorCall "Prelude.!!: index too large"
            it "selects the first n elements of a list (safe)" $ do
                take 3 [1, 2, 3, 4, 5] `shouldBe` [1, 2, 3]
                take 99 [1, 2] `shouldBe` [1, 2]
                take 99 ([] :: [Int]) `shouldBe` []
            it "can append to a list with `++` anotherList" $ do
                [1, 2, 3] ++ [4, 5] `shouldBe` [1 .. 5]
            it "computes the size of a list with `length`" $ do
                length [1, 2, 3] `shouldBe` 3
            it "can sum elements of a list ([] = 0)" $ do
                sum [1 .. 4] `shouldBe` 10 -- 1 + 2 + 3 + 4
                sum [] `shouldBe` 0
            it "can multiply elements of a list ([] = 1, UNSAFE??)" $ do
                product [1 .. 4] `shouldBe` 24 -- 1 * 2 * 3 * 4
                product [] `shouldBe` 1
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
                    replicate 3 'b' `shouldBe` "bbb" -- better!
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