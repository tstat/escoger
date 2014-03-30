{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.Reader
import           Control.Monad.State.Strict
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V
import           Escoger.Interactive
import           Escoger.Internal
import           Graphics.Vty
import           System.Posix.IO

main :: IO ()
main = do
  content <- BC.getContents
  let content' = (V.filter (not . BC.null) . V.fromList . BC.split '\n') content
  ifd <- openFd "/dev/tty" ReadOnly Nothing defaultFileFlags
  ofd <- openFd "/dev/tty" WriteOnly Nothing defaultFileFlags
  vty <- mkVty defaultConfig { inputFd = Just ifd, outputFd = Just ofd }
  selection <- flip evalStateT (SearchState content' "" 1) $
    flip runReaderT (SearchData content' vty) $
      interactiveLoop
  BC.putStrLn selection
