{-# LANGUAGE OverloadedStrings #-}

module Escoger.Utils where

import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Escoger.Internal (maxRows)

safeInit :: ByteString -> ByteString
safeInit "" = ""
safeInit bs = BC.init bs

safePred :: Int -> Int
safePred 1 = 1
safePred x = pred x

safeSucc :: Int -> Int -> Int
safeSucc x y = if c then y else succ y
  where c = or $ [(x==), (maxRows==)] <*> [y]
