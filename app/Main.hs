module Main (main) where

import qualified Enumerative

main :: IO ()
main = do
  print $ Enumerative.search [ ("Michael Chavinda", "Michael.Chavinda")
                             , ("Shaka Zulu", "Shaka.Zulu")] 5
