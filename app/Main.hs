module Main (main) where

import qualified MyLib

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  print $ MyLib.search "Michael Chavinda" "Michael.Chavinda" 4
