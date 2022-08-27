module RegexTest (spec) where

import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Text.Regex.PCRE.Heavy (Regex, gsub, re, scan, (=~))

spec :: Spec
spec =
    describe "Regexes" $ do
        describe "DELETE_ME" $ do
            it "works" $ do
                1 + 1 `shouldBe` 3

{-

regex1 :: Regex
regex1 = [re|^(hell.), (.+)!$|]

-- I'm not sure why I have to provide extra type information here...
-- Not necessary inside the REPL!
--
-- Normal usage:
--   "hello, world!" =~ regex1 => True
--
-- Reverse the call order:
--   (\x -> x =~ regex1) "hello, world!"
--      => True
--
--   compare = \x -> (x :: Text) =~ regex1
--
--   compare "hello, world!"
--     => True
reCompare :: Bool
reCompare = ("hello" :: String) =~ regex1

reCompare2 :: Bool
reCompare2 = ("hello" :: [Char]) =~ regex1

{-
scan regex1 ("hello, world!" :: Text)
  => [("hello, world!",["hello","world"])]
-}
extractFromRe :: [(String, [String])]
extractFromRe = scan regex1 "hello, world!"

{-
The REPL wants the type hint in reverse order it seems...
gsub [re|\d+|] "X" ("Remove 123 OK" :: Text)
    => "Remove X OK"
-}
substituteFromRe :: String
substituteFromRe = gsub [re|\d+|] ("X" :: String) "Remove 123 OK"

{-
The REPL wants the type hint in reverse order it seems...
gsub [re|\d|] "x" ("Remove 123 OK" :: Text)
    => "Remove xxx OK"
-}
substituteFromReNonGreedy :: String
substituteFromReNonGreedy = gsub [re|\d|] ("x" :: String) "Remove 123 OK"

main :: IO ()
main = hspec $ do
    describe "Regex" $ do
        it "works" $ do
            1 + 1 `shouldBe` 2

 -}