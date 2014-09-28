{-# LANGUAGE DataKinds #-}
module WhatIsThisGame.Rendering ( generateVertices
                                , renderQuads
                                , renderSpriteBatch
                                ) where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL hiding (Shader)
import Data.Foldable hiding (foldl1)
import Control.Applicative
import Graphics.VinylGL
import Graphics.GLUtil
import Data.Vinyl
import Linear.V4
import Linear.V2

-------------------
-- Local Imports --
import WhatIsThisGame.Data

----------
-- Code --

-- | Generating a list of vertices for a quad based on a position and a size.
generateVertices :: (Real a, Fractional b) => V2 a -> V2 a -> [V2 b]
generateVertices p s =
  V2 <$> [x, x + w] <*> [y, y + h]
  where (V2 x y) = fmap realToFrac p
        (V2 w h) = fmap realToFrac s

-- | Compute a textured vertex record for each input vertex.
tileTex :: [[V2 GLfloat]] -> [PlainFieldRec [VertexCoord, TextureCoord]]
tileTex =
  foldMap (flip (zipWith (<+>)) (cycle coords) . map (vertexCoord =:))
  where coords = map (textureCoord =:) $ V2 <$> [0, 1] <*> [1, 0]

-- | Computing the untextured vertex record for each input vertex.
calcQuad :: [[V2 GLfloat]] -> [PlainFieldRec [VertexCoord, GLSLColor]]
calcQuad =
  foldMap (flip (zipWith (<+>)) (cycle coords) . map (vertexCoord =:))
  where coords = map (glslColor =:) [ V4 1 0 0 1
                                    , V4 0 1 0 1
                                    , V4 0 0 1 1
                                    , V4 1 0 1 1
                                    ]

-- | Calculating the number of indices required for a list.
calcIndices :: (Enum b, Num b) => [a] -> [b]
calcIndices l = take (6 * length l) $ foldMap (flip map [0, 1, 2, 2, 1, 3] . (+)) [0, 4..]

-- | Rendering a set of quads.
renderQuads :: CamMatrix -> Shader -> [(V2 Float, V2 Float)] -> IO ()
renderQuads cm (Shader sp) l = do
  verts <- bufferVertices $ calcQuad $ map (uncurry generateVertices) l
  eb    <- bufferIndices $ calcIndices l
  vao   <- makeVAO $ do
    enableVertices' sp verts
    bindVertices verts
    bindBuffer ElementArrayBuffer $= Just eb

  currentProgram $= Just (program sp)
  setUniforms sp cm
  withVAO vao $ drawIndexedTris 2

  deleteVertices verts
  deleteObjectName eb
  deleteVAO vao

-- | Rendering a bunch of sprites.
renderTextures :: CamMatrix -> Shader -> [(Sprite, V2 Float, V2 Float)] -> IO ()
renderTextures cm (Shader sp) l = do
  let tos = map (\((Sprite a), _, _) -> a) l
      ps  = map (\(_, a, _) -> a) l
      ss  = map (\(_, _, a) -> a) l

  verts <- bufferVertices $ tileTex $ map (uncurry generateVertices) $ zip ps ss
  eb    <- bufferIndices $ calcIndices l
  vao   <- makeVAO $ do
    enableVertices' sp verts
    bindVertices verts
    bindBuffer ElementArrayBuffer $= Just eb

  currentProgram $= Just (program sp)
  setUniforms sp cm
  withVAO vao . withTextures2D tos $ drawIndexedTris 2

  deleteVertices verts
  deleteObjectName eb
  deleteVAO vao

-- | Making a list of three-tuples forming the input for @'renderTextures'@
--   from a @'SpriteRender'@.
toData :: SpriteRender -> [(Sprite, V2 Float, V2 Float)]
toData (SpriteRender  t p w) = [(t, p, w)]
toData (SpriteRenders l    ) = foldl1 (++) $ map toData l

-- | Rendering a @'SpriteRender'@.
renderSpriteBatch :: CamMatrix -> Shader -> SpriteRender -> IO ()
renderSpriteBatch cm sp = renderTextures cm sp . toData
