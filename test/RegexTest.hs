module RegexTest (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Regex.PCRE.Heavy (gsub, re, scan, (=~))

spec :: Spec
spec =
    describe "Regexes" $ do
        describe "^(hell.), (.+)!$" $ do
            let regex = [re|^(hell.), (.+)!$|]
            it "matches: \"hello, world!\"" $ do
                "hello, world!" =~ regex `shouldBe` True
            it "does not match: \"hello!\"" $
                "hello!" =~ regex `shouldBe` False
            it "can be called in reverse order" $
                -- (\x -> x =~ regex) "hello, world!" `shouldBe` True
                (=~ regex) "hello, world!" `shouldBe` True
            it "can be stored in a variable for latter use" $ do
                let match = (=~ regex)
                match "hello, world!" `shouldBe` True
                match "hello!" `shouldBe` False
            it "can scan a string" $ do
                scan regex "hello, world!" `shouldBe` [("hello, world!", ["hello", "world"])]
                scan regex "hello!" `shouldBe` []
            it "can capture matches" $ do
                scan [re|a(b).|] "hello abc, and abd" `shouldBe` [("abc", ["b"]), ("abd", ["b"])]
                scan [re|a(b)[cd]|] "hello abc, and abd" `shouldBe` [("abc", ["b"]), ("abd", ["b"])]
            it "can replace with gsub" $ do
                gsub [re|\d+|] "X" "Remove 123 OK" `shouldBe` "Remove X OK"
                gsub [re|\d|] "x" "Remove 123 OK" `shouldBe` "Remove xxx OK"