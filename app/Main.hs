module Main (main) where

import qualified Enumerative

main :: IO ()
main = do
    print $
        Enumerative.search
            [ ("Michael Chavinda", "Michael")
            , ("Shaka Zulu", "Shaka")
            ]
            5
