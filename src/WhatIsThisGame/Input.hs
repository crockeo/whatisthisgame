-- | A module for dealing with input from GLFW. Both globally in the IO monad
--   and within the Elerea network.
module WhatIsThisGame.Input where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW as GLFW
import Control.Wire
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
renderSize :: Fractional b => Wire s e IO a (V2 b)
renderSize =
  mkGen_ $ \_ -> do
    rs <- ioRenderSize
    return $ Right rs

-- | Getting the render size of the window in the Elerea network.
keyDown :: (Enum k, Monoid e) => k -> Wire s e IO a a
keyDown k =
  mkGen_ $ \a -> do
    k <- getKey k
    return $ case k of
      Release -> Left  mempty
      Press   -> Right a
