-- | A module for dealing with input from GLFW. Both globally in the IO monad
--   and within the Elerea network.
module WhatIsThisGame.Input where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW as GLFW
import FRP.Elerea.Param
import System.Random
import Linear.V2

----------
-- Code --

-- | Getting the render size of the window in the IO monad.
ioRenderSize :: Fractional a => IO (V2 a)
ioRenderSize =
  fmap convert $ get windowSize
  where convert :: Fractional a => Size -> V2 a
        convert (Size w h) =
          V2 (fromIntegral w / 640 * 100)
             (fromIntegral h / 640 * 100)

-- | Getting the render size of the window in the Elerea network.
renderSize :: Fractional a => SignalGen p (Signal (V2 a))
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

-- | Getting a random value within a range.
randomRGen :: Random a => (a, a) -> SignalGen p a
randomRGen p =
  execute $ randomRIO p
