{-# LANGUAGE OverloadedStrings #-}

module Unit.Matches (unit_matches) where

import qualified Data.Vector as V
import           Escoger.Matches
import           Test.Framework (defaultMain, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit

shorter_terms = assertEqual "favors shorter terms" correctOrder resultOrder
  where
    correctOrder = ["kite", "kitch", "kitten"]
    resultOrder = V.toList . sortByScore "kit" $ V.fromList ["kitten", "kitch", "kite"]

ordered_terms = assertEqual "ignores out of order terms" correctOrder resultOrder
  where
    correctOrder = ["ectwdaokxycaojxh"]
    resultOrder = V.toList . sortByScore "each" $ V.fromList ["aech", "ectwdaokxycaojxh"]

ignores_case = assertEqual "ignores case" correctOrder resultOrder
  where
    correctOrder = ["Each", "eachr"]
    resultOrder = V.toList . sortByScore "each" $ V.fromList ["eachr", "Each"]

unit_matches = testGroup "matches"
               [ testCase "favor shorter terms" shorter_terms
               , testCase "ignore out of order terms" ordered_terms
               , testCase "ignores case" ignores_case
               ]
