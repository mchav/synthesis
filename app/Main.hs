module Main (main) where

import qualified Enumerative

main :: IO ()
main = do
    print $
        Enumerative.search
            [ ("Michael Chavinda", "Michael.Chavinda")
            , ("Steve Biko", "Steve.Biko")
            , ("Joshua Nkomo", "Joshua.Nkomo")
            ]
            4
