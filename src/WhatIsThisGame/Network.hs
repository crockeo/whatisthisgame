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
import Control.Wire
import Data.Vinyl
import Data.IORef

-------------------
-- Local Imports --
import WhatIsThisGame.Rendering
import WhatIsThisGame.Assets
import WhatIsThisGame.Data

----------
-- Code --

-- | The backend to running the network.
runNetwork' :: Renderable b => IORef Bool -> Camera GLfloat -> Assets -> Session IO (Timed NominalDiffTime ()) -> Wire (Timed NominalDiffTime ()) e IO a b -> IO ()
runNetwork' closedRef cam assets session wire = do
  closed <- readIORef closedRef
  if closed
    then return ()
    else do
      (st, session') <- stepSession session
      (wt, wire'   ) <- stepWire    wire st $ Right undefined

      case wt of
        Left  _ -> return ()
        Right w -> do
          let cm = (SField =: camMatrix cam)
              sp = (getShaders assets ! "res/game2d", getShaders assets ! "res/color")
              r  = render assets w

          clear [ColorBuffer, DepthBuffer]
          performRender cm sp r
          swapBuffers

          threadDelay 16666
          runNetwork' closedRef cam assets session' wire'

-- | Running the network.
runNetwork :: Renderable b => IORef Bool -> Wire (Timed NominalDiffTime ()) e IO a b -> IO ()
runNetwork closedRef wire = do
  assets <- loadAssets

  GLFW.time $= 0
  runNetwork' closedRef camera2D assets clockSession_ wire
