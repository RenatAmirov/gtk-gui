
module Main (main) where

import Control.Monad
import Graphics.UI.Gtk

main :: IO ()
main = do
  void initGUI
  window <- renderWindow
  display <- renderDisplay

  grid <- gridNew
  gridSetRowHomogeneous grid True
  containerAdd window grid

  let attach x y w h item = gridAttach grid item x y w h
  attach 0 0 5 1 display
  
  -- Add just one test button
  button <- buttonNewWithLabel "Test"
  attach 0 1 1 1 button
  {-
  on button buttonPressed $ putStrLn "Button clicked!"
  on window objectDestroy mainQuit
  -}
 
  widgetShowAll window
  mainGUI

renderDisplay :: IO Entry
renderDisplay = do
  display <- entryNew
  set display [ entryEditable := False  
             , entryXalign    := 1  -- right-alignment
             , entryText := "0" ]
  pure display

renderWindow :: IO Window
renderWindow = do
  window <- windowNew
  set window [ windowTitle         := "Calculator"
            , windowResizable     := False
            , windowDefaultWidth  := 230
            , windowDefaultHeight := 250 ]
  pure window


{-
module Main (main) where

import Graphics.UI.Gtk

main :: IO ()
main = do
  void initGUI
  
  window <- windowNew
  set window [ windowTitle := "Calculator"
             , windowDefaultWidth := 300
             , windowDefaultHeight := 400 ]
  
  grid <- gridNew
  gridSetRowHomogeneous grid True
  gridSetColumnHomogeneous grid True
  containerAdd window grid
  
  display <- entryNew
  set display [ entryEditable := False
              , entryXalign := 1
              , entryText := "0" ]
  
  let attach x y w h item = gridAttach grid item x y w h
  attach 0 0 5 1 display
  
  -- Add just one test button
  button <- buttonNewWithLabel "Test"
  attach 0 1 1 1 button
  
  on button buttonPressed $ putStrLn "Button clicked!"
  on window objectDestroy mainQuit
  
  widgetShowAll window
  mainGUI



  let attach x y w h item = gridAttach grid item x y w h
  attach 0 0 5 1 display
    mkBtn "MC" >>= attach 0 1 1 1

-}