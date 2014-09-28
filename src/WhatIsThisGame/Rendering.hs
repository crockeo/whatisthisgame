module WhatIsThisGame.Rendering (renderSpriteBatch) where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL
import Graphics.GLUtil
import Linear.V2

-------------------
-- Local Imports --
import WhatIsThisGame.Data

----------
-- Code --

-- | Rendering a bunch of sprites.
renderTextures :: CamMatrix -> ShaderProgram -> [(TextureObject, V2 Float, V2 Float)] -> IO ()
renderTextures _ _ _ = undefined

-- | Making a list of three-tuples forming the input for @'renderTextures'@
--   from a @'SpriteRender'@.
toData :: SpriteRender -> [(TextureObject, V2 Float, V2 Float)]
toData (SpriteRender  t p w) = [(t, p, w)]
toData (SpriteRenders l    ) = foldl1 (++) $ map toData l

-- | Rendering a @'SpriteRender'@.
renderSpriteBatch :: CamMatrix -> ShaderProgram -> SpriteRender -> IO ()
renderSpriteBatch cm sp = renderTextures cm sp . toData