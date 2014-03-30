module Escoger.Internal where

import Control.Monad.State.Strict
import Control.Monad.Reader
import Data.ByteString (ByteString)
import Data.Vector (Vector)
import Graphics.Vty (Vty)

data SearchState = SearchState { _matches :: Vector ByteString
                               , _term    :: ByteString
                               , _index   :: Int
                               } deriving (Show)

data SearchData = SearchData { _content :: Vector ByteString
                             , _vty     :: Vty
                             }

type SearchM a = ReaderT SearchData (StateT SearchState IO) a

type Index = Int

maxRows :: Int
maxRows = 100
