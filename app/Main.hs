module Main where

import qualified Tree (hello)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  putStrLn Tree.hello
