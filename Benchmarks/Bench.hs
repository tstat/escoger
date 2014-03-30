{-# LANGUAGE OverloadedStrings #-}

import Escoger.Matches
import Criterion.Main
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V

main :: IO ()
main = do
  content <- BC.readFile "Benchmarks/Random.txt"
  let content' = V.fromList $ BC.split '\n' content
  defaultMain [
      bench "sortByScore" $ nf (sortByScore "aeu") content'
    ]
