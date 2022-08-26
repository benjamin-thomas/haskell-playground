module Main where

import qualified ExplorePrelude (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  ExplorePrelude.someFunc
