-- | This module contains functionality for running a given Elerea FRP network
--   so long as its param is a float and its result can be rendered.
module WhatIsThisGame.Network (runNetwork) where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL
import Graphics.GLUtil.Camera2D
import Graphics.UI.GLFW as GLFW
import Data.Vinyl.Universe
import FRP.Elerea.Param
import Data.Vinyl
import Data.IORef

-------------------
-- Local Imports --
import WhatIsThisGame.Assets
import WhatIsThisGame.Data

----------
-- Code --

-- | The backend to running the network.
runNetwork' :: Renderable a => IORef Bool -> Camera GLfloat -> Assets -> (Float -> IO a) -> IO ()
runNetwork' closedRef cm assets sfn = do
  closed <- readIORef closedRef
  if closed
    then return ()
    else do
      a <- get GLFW.time >>= sfn . realToFrac
      GLFW.time $= 0

      clear [ColorBuffer, DepthBuffer]
      render (SField =: camMatrix cm) assets a
      swapBuffers

      runNetwork' closedRef cm assets sfn

-- | Running the network.
runNetwork :: Renderable a => IORef Bool -> SignalGen Float (Signal a) -> IO ()
runNetwork closedRef sg = do
  assets <- loadAssets
  sfn    <- start sg

  GLFW.time $= 0
  runNetwork' closedRef camera2D assets sfn
