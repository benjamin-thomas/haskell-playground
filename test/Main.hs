module Main (main) where

{-
NOTE:

    `cabal test` **wants** a `Main` module with `main` function.
    So I can't name this module `Spec` as per HSpec's documented conventions.
-}

import qualified ExplorePreludeTest as ExplorePrelude
import qualified ExplorePreludeTest as RegexTest
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
    -- ExplorePrelude.spec
    RegexTest.spec
