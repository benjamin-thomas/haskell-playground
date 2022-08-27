module ExceptionHandlingTest where

import Control.Exception (evaluate)
import ExceptionHandling (
    ServerException (ServerOnFireException),
    catchFire,
    isBelow10,
    isBelow10R,
    runAndCatch,
 )

import Test.Hspec (
    Spec,
    describe,
    errorCall,
    it,
    shouldBe,
    shouldThrow,
 )

spec :: Spec
spec =
    describe "Exception handling" $ do
        describe "An impure exception" $ do
            it "does not raise on good input" $ do
                isBelow10 9 `shouldBe` True
            it "raises on bad input (UNSAFE)" $ do
                evaluate (isBelow10 10) `shouldThrow` errorCall "above or equal to 10!"
        describe "using `Either` as a result type can still fail" $ do
            it "does not raise on good input" $ do
                isBelow10R 9 `shouldBe` Right ()
            it "defers raising on bad input due to laziness" $ do
                let res = isBelow10R 10 -- exception not thrown here, lazy!
                1 `shouldBe` (1 :: Int) -- program would happily continue if not evaluated below
                evaluate
                    ( case res of
                        Left _err ->
                            error "could rethrow"
                        Right () ->
                            error "impossible"
                    )
                    `shouldThrow` errorCall "could rethrow"
        describe "catching exceptions" $ do
            it "catches fire!" $ do
                evaluate catchFire `shouldThrow` (== ServerOnFireException)
            it "catches the... fire caught ;)" $ do
                runAndCatch catchFire
                1 `shouldBe` (1 :: Int) -- This is fine...
