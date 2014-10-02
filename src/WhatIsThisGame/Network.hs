-- | This module contains functionality for running a given Elerea FRP network
--   so long as its param is a float and its result can be rendered.
module WhatIsThisGame.Network (runNetwork) where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL
import Graphics.GLUtil.Camera2D
import Graphics.UI.GLFW as GLFW
import Data.Vinyl.Universe
import Control.Concurrent
import FRP.Elerea.Param
import Data.Vinyl
import Data.IORef
import Linear.V2

-------------------
-- Local Imports --
import WhatIsThisGame.Rendering
import WhatIsThisGame.Assets
import WhatIsThisGame.Input
import WhatIsThisGame.Data

----------
-- Code --

-- | Calculating the distance that the camera should track on a given udpate.
trackDistance :: Float -> IO (V2 GLfloat)
trackDistance dt = do
  (V2 rw _) <- ioRenderSize
  return $ V2 (realToFrac $ playerMoveSpeed * dt / (rw / 2))
              0

-- | The backend to running the network.
runNetwork' :: Renderable a => IORef Bool -> Camera GLfloat -> Assets -> (Float -> IO a) -> IO ()
runNetwork' closedRef cam assets sfn = do
  closed <- readIORef closedRef
  if closed
    then return ()
    else do
      dt <- fmap realToFrac $ get GLFW.time
      GLFW.time $= 0
      a <- sfn dt

      let cm = (SField =: camMatrix cam)
          sp = (getShaders assets ! "res/game2d", getShaders assets ! "res/color")
          r  = render assets a


      clear [ColorBuffer, DepthBuffer]
      performRender cm sp r
      swapBuffers

      threadDelay 16666
      td <- trackDistance dt
      runNetwork' closedRef (track td cam) assets sfn

-- | Running the network.
runNetwork :: Renderable a => IORef Bool -> SignalGen Float (Signal a) -> IO ()
runNetwork closedRef sg = do
  assets <- loadAssets
  sfn    <- start sg

  GLFW.time $= 0
  runNetwork' closedRef camera2D assets sfn
