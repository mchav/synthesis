module Main (main) where

import qualified Enumerative

main :: IO ()
main = do
  print $ Enumerative.search [ ("Michael Chavinda", "M.Chavinda")
                             , ("Shaka Zulu", "S.Zulu")] 5
