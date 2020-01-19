module Main where

import qualified MyLib (defaultMain)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  MyLib.defaultMain
