module Main where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW as GLFW
import Data.IORef

-------------------
-- Local Imports --
import WhatIsThisGame.WindowConfig
import WhatIsThisGame.Network
import WhatIsThisGame.World

----------
-- Code --

-- | Making a callback for window close.
makeWindowCloseCallback :: IORef Bool -> WindowCloseCallback
makeWindowCloseCallback closedRef = do
  writeIORef closedRef True
  return True

-- | The entry point to the application.
main :: IO ()
main = do
  wc <- loadGuaranteed "config.cfg"

  initialize
  openWindowHint NoResize True
  openWindow (makeSize wc)
             [DisplayRGBBits 8 8 8, DisplayAlphaBits 8, DisplayDepthBits 24]
             (makeWindowMode wc)
  windowTitle $= "What is this game?"

  closedRef <- newIORef False
  windowCloseCallback $= makeWindowCloseCallback closedRef

  blend     $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)

  runNetwork closedRef world

  closeWindow
