{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
module WhatIsThisGame.Rendering (performRender) where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL hiding ( Shader
                                        , Color
                                        )

import Data.Foldable hiding (foldl1, mapM_)
import Graphics.Rendering.FTGL
import Control.Applicative
import Graphics.VinylGL
import Graphics.GLUtil
import Data.Vinyl
import Linear.V2

-------------------
-- Local Imports --
import WhatIsThisGame.Input
import WhatIsThisGame.Data

----------
-- Code --

-- | Scaling a coordinate to the window size.
scaleCoord :: (Real a, Fractional b) => V2 a -> IO (V2 b)
scaleCoord (V2 x y) = do
  (V2 w h) <- ioRenderSize
  return $ V2 (realToFrac x / w)
              (realToFrac y / h)
-- | Generating a list of vertices for a quad based on a position and a size.
generateVertices :: (Real a, Fractional b) => V2 a -> V2 a -> IO [V2 b]
generateVertices p s = do
  (V2 x y) <- scaleCoord p
  (V2 w h) <- scaleCoord s
  return $ V2 <$> [x, x + w] <*> [y, y + h]

-- | Calculating the number of indices required for a list.
calcIndices :: (Enum b, Num b) => Int -> [b]
calcIndices n = take (6 * n) $ foldMap (flip map [0, 1, 2, 2, 1, 3] . (+)) [0, 4..]

-- | Computing the texture coordinate for each input coordinate.
tileTex :: [[V2 GLfloat]] -> [PlainFieldRec [VertexCoord, TextureCoord]]
tileTex =
  foldMap (flip (zipWith (<+>)) (cycle coords) . map (vertexCoord =:))
  where coords = map (textureCoord =:) $ V2 <$> [0, 1] <*> [1, 0]

-- | Rendering a set of @'Sprite'@s contained within a list of @'Render'@
--   calls.
actuallyPerformRender :: CamMatrix -> Shader -> SpriteBatch -> IO ()
actuallyPerformRender cm (Shader sp) (SpriteBatch rs) = do
  let tos = map (\(SpriteRender (Sprite a) _ _) -> a) rs
      ps  = map (\(SpriteRender         _  a _) -> a) rs
      ss  = map (\(SpriteRender         _  _ a) -> a) rs
      len = length rs

  verts <- mapM (uncurry generateVertices) (zip ps ss) >>= bufferVertices . tileTex
  eb    <- bufferIndices $ calcIndices len
  vao   <- makeVAO $ do
    enableVertices' sp verts
    bindVertices verts
    bindBuffer ElementArrayBuffer $= Just eb

  currentProgram $= Just (program sp)
  setUniforms sp cm
  withVAO vao . withTextures2D tos $ drawIndexedTris (2 * fromIntegral len)

  currentProgram $= Nothing
  deleteVertices verts
  deleteObjectName eb
  deleteVAO vao

-- | Rendering a @'SpriteBatch'@.
renderSpriteBatch :: CamMatrix -> Shader -> SpriteBatch -> IO ()
renderSpriteBatch = actuallyPerformRender

-- | Rendering a @'TextRender'@.
renderText :: CamMatrix -> Shader -> TextRender -> IO ()
renderText cm (Shader sp) (TextRender font string _ _) = do
  setFontFaceSize font 1 1
  renderFont font string All

-- | Performing a bunch of @'Renders'@.
performRender :: CamMatrix -> (Shader, Shader) -> Renders -> IO ()
performRender  _ _    []  = return ()
performRender cm sp@(ss, ts) (r:rs) = do
  case r of
    (RenderSprite  sr) -> renderSpriteBatch cm ss $ SpriteBatch [sr]
    (RenderSprites sb) -> renderSpriteBatch cm ss  sb
    (RenderText    tr) -> renderText        cm ts  tr
  performRender cm sp rs
