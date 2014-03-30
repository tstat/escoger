module Escoger.Matches
( sortByScore
, score
, matchLength
, findEndOfMatch
) where

import           Control.Applicative
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import           Data.Char (ord, chr)
import           Data.Maybe (mapMaybe)
import           Data.Ord (comparing)
import           Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VA
import           GHC.Unicode (isAsciiUpper)

sortByScore :: ByteString -> Vector ByteString -> Vector ByteString
sortByScore query choices = sortFilter choiceScores
  where
    sortFilter = V.map fst . V.modify (VA.sortBy $ comparing (negate . snd)) . V.filter (\(_,y) -> y /= 0)
    choiceScores = V.map (\x -> (x, score x query)) choices

score :: ByteString -> ByteString -> Double
score choice query
  | B.null query = 1
  | B.null choice = 0
  | otherwise = case matchLength c q of
    Nothing -> 0
    Just len -> let tmpScore = fromIntegral (B.length q) / fromIntegral len
                    choiceLength = fromIntegral $ B.length c
                in tmpScore / choiceLength
    where
      c = BC.map toAsciiLower choice
      q = BC.map toAsciiLower query

matchLength :: ByteString -> ByteString -> Maybe Int
matchLength choice query =
  if null matches then Nothing else Just $ minimum matches
  where
    matches :: [Int]
    matches = (filter (0<=) . mapMaybe findNorm) indices
    findNorm i = findEndOfMatch choice query i >>= normalize i
    normalize i m = return $ 1 + subtract i m
    indices :: [Int]
    indices = B.elemIndices (B.head query) choice

findEndOfMatch :: ByteString -> ByteString -> Int -> Maybe Int
findEndOfMatch choice query index =
  B.foldl' func (Just index) (B.tail query)
  where
    func m_lastIndex q = let findNextMatch i = B.findIndex (q ==) (B.drop (i+1) choice)
                             acc = ((+) <$> ((+1) <$> m_lastIndex) <*>)
                    in m_lastIndex >>= findNextMatch >>= acc . return

toAsciiLower :: Char -> Char
toAsciiLower c = if isAsciiUpper c
                   then chr (ord c + 32)
                   else c
