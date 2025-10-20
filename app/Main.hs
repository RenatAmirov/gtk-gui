module Main (main) where

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk hiding (Action, backspace)

main :: IO ()
main = do
  void initGUI          -- (1)
  window <- windowNew   -- (2)
                        -- (3)
  widgetShowAll window  -- (4)
  mainGUI               -- (5)

