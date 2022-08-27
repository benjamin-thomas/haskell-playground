module Main (main) where

{-
NOTE:

    `cabal test` **wants** a `Main` module with `main` function.
    So I can't name this module `Spec` as per HSpec's documented conventions.

NOTE:

    Override the default format with:

        cabal test --test-option=--format=progress
-}

import qualified ExceptionHandlingTest
import qualified ExplorePreludeTest
import qualified RegexTest
import Test.Hspec (hspec)
import qualified TimeTest

main :: IO ()
main = hspec $ do
    -- When working on a new module, use something like: ./manage/reload_repl_on_change Regexes
    ExplorePreludeTest.spec
    RegexTest.spec
    ExceptionHandlingTest.spec
    TimeTest.spec
