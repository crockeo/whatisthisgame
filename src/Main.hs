module Main where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW as GLFW
import Data.IORef

-------------------
-- Local Imports --
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
  initialize
  openWindow (Size 640 480) [DisplayRGBBits 8 8 8, DisplayAlphaBits 8, DisplayDepthBits 24] Window
  windowTitle $= "What is this game?"

  closedRef <- newIORef False
  windowCloseCallback $= makeWindowCloseCallback closedRef

  runNetwork closedRef world

  closeWindow
