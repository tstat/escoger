{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Escoger.Interactive (interactiveLoop) where

import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import           Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import           Escoger.Internal
import           Escoger.Matches (sortByScore)
import           Escoger.Utils
import           Graphics.Vty
import           System.Exit (exitSuccess)

interactiveLoop :: SearchM ByteString
interactiveLoop = do
  render
  loop
  where
    loop :: SearchM ByteString
    loop = do
      vty <- asks _vty
      st <- get
      e <- liftIO $ nextEvent vty
      let result = handleKeyPress e st
      case result of
        Right selectionIndex -> do
          liftIO $ shutdown vty
          if selectionIndex > 0
          then do
            matches <- gets _matches
            return $ fromMaybe "" $ matches V.!? (selectionIndex - 1)
          else liftIO exitSuccess
        Left ss -> do
          updateMatches ss
          render
          loop

render :: SearchM ()
render = do
  vty <- asks _vty
  term <- gets _term
  index <- gets _index
  matches <- gets _matches
  let termLine = utf8Bytestring' defAttr $ mconcat ["> ", term]
      imatches = (V.map (formatMatch index) . V.zip [1..maxRows] . V.take maxRows) matches
      img = V.foldl' vertJoin termLine imatches
      pic = picForImage img
  liftIO $ update vty pic
  where
    formatMatch :: Index -> (Index,ByteString) -> Image
    formatMatch i (x,y) = if i == x
                            then utf8Bytestring' (defAttr `withBackColor` white `withForeColor` black) y
                            else utf8Bytestring' defAttr y

handleKeyPress :: Event -> SearchState -> Either SearchState Index
handleKeyPress e ss@(SearchState m t i) = do
  case e of
    (EvKey KBS []) -> Left $ ss { _term = safeInit t }
    (EvKey KDel []) -> Left $ ss { _term = safeInit t }
    (EvKey KUp []) -> Left $ ss { _index = safePred i }
    (EvKey KDown []) -> Left $ ss { _index = safeSucc' i }
    (EvKey KEnter []) -> Right i
    (EvKey (KChar 'p') [MCtrl]) -> Left $ ss { _index = safePred i }
    (EvKey (KChar 'n') [MCtrl]) -> Left $ ss { _index = safeSucc' i }
    (EvKey (KChar 'u') [MCtrl]) -> Left $ ss { _term = "" }
    (EvKey (KChar 'w') [MCtrl]) -> Left $ ss { _term = (BC.unwords . init . BC.words) t }
    (EvKey (KChar 'c') [MCtrl]) -> Right (-1)
    (EvKey (KChar c) []) -> Left $ ss { _index = 1, _term = BC.snoc t c }
    (EvKey KEsc []) -> Right (-1)
    _ -> Left ss
  where
    safeSucc' = safeSucc (V.length m)

updateMatches :: SearchState -> SearchM ()
updateMatches (SearchState _ t i) = do
  st <- get
  term <- gets _term
  if B.length t /= B.length term
    then do
      terms <- if BC.length term < BC.length t then gets _matches else asks _content
      put $ st { _index = i, _term = t, _matches = sortByScore t terms }
    else put $ st { _index = i }
