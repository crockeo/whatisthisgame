-- | A module for dealing with input from GLFW. Both globally in the IO monad
--   and within the Elerea network.
module WhatIsThisGame.Input where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW as GLFW
import FRP.Elerea.Param
import Linear.V2

----------
-- Code --

-- | Getting the render size of the window in the IO monad.
ioRenderSize :: IO (V2 Float)
ioRenderSize =
  fmap convert $ get windowSize
  where convert :: Size -> V2 Float
        convert (Size w h) =
          V2 (fromIntegral w)
             (fromIntegral h)

-- | Getting the render size of the window in the Elerea network.
renderSize :: SignalGen p (Signal (V2 Float))
renderSize = effectful ioRenderSize

-- | Checking if a key is held down in the IO monad.
ioKeyDown :: Enum k => k -> IO Bool
ioKeyDown k = do
  s <- getKey k
  return $ case s of
    Release -> False
    Press   -> True

-- | Getting the render size of the window in the Elerea network.
keyDown :: Enum k => k -> SignalGen p (Signal Bool)
keyDown = effectful . ioKeyDown
