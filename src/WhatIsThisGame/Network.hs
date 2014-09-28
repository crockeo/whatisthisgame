-- | This module contains functionality for running a given Elerea FRP network
--   so long as its param is a float and its result can be rendered.
module WhatIsThisGame.Network (runNetwork) where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW as GLFW
import FRP.Elerea.Param
import Data.IORef

-------------------
-- Local Imports --
import WhatIsThisGame.Data

----------
-- Code --

-- | The backend to running the network.
runNetwork' :: Renderable a => IORef Bool -> (Float -> IO a) -> IO ()
runNetwork' closedRef sfn = do
  closed <- readIORef closedRef
  if closed
    then return ()
    else do
      a <- get GLFW.time >>= sfn . realToFrac
      GLFW.time $= 0

      clear [ColorBuffer, DepthBuffer]
      render a
      swapBuffers

      runNetwork' closedRef sfn

-- | Running the network.
runNetwork :: Renderable a => IORef Bool -> SignalGen Float (Signal a) -> IO ()
runNetwork closedRef sg = do
  GLFW.time $= 0
  start sg >>= runNetwork' closedRef
