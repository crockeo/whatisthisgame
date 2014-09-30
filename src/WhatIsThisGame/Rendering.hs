{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
module WhatIsThisGame.Rendering (performRender) where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL hiding ( Shader
                                        , Color
                                        )

import Data.Foldable hiding (foldl1)
import Control.Applicative
import Data.Vinyl.TyFun
import Graphics.VinylGL
import Graphics.GLUtil
import Control.Monad
import Data.Vinyl
import Linear.V4
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

-- | Binding a color.
bindColor :: (App el GLSLColor ~ V4 GLfloat, Applicative f) => Color -> Rec el f '[GLSLColor]
bindColor (Color c) = glslColor =: fmap realToFrac c

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

-- | Computing the color for each input coordinate.
calcQuad :: [Color] -> [[V2 GLfloat]] -> [PlainFieldRec [VertexCoord, GLSLColor]]
calcQuad cs = foldMap (flip (zipWith (<+>)) (cycle $ map bindColor cs) . map (vertexCoord =:))

-- | Rendering a set of @'Sprite'@s contained within a list of @'Render'@
--   calls.
renderSprites :: CamMatrix -> Shader -> [Render] -> IO ()
renderSprites cm (Shader sp) rs = do
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

  deleteVertices verts
  deleteObjectName eb
  deleteVAO vao

-- | Rendering a set of quads contained within a list of @'Render'@ calls.
renderQuads :: CamMatrix -> Shader -> [Render] -> IO ()
renderQuads cm (Shader sp) rs = do
  let cs  = foldl1 (++) $ map (\(QuadRender a _ _) -> a) rs
      ps  = map (\(QuadRender _ a _) -> a) rs
      ss  = map (\(QuadRender _ _ a) -> a) rs
      len = length rs

  verts <- mapM (uncurry generateVertices) (zip ps ss) >>= bufferVertices . calcQuad cs
  eb    <- bufferIndices $ calcIndices len
  vao   <- makeVAO $ do
    enableVertices' sp verts
    bindVertices verts
    bindBuffer ElementArrayBuffer $= Just eb

  currentProgram $= Just (program sp)
  setUniforms sp cm
  withVAO vao $ drawIndexedTris (fromIntegral len * 2)

  deleteVertices verts
  deleteObjectName eb
  deleteVAO vao

  return ()

-- | Performing a render on a whole @'Render'@ call.
performRender :: CamMatrix -> (Shader, Shader) -> Render -> IO ()
performRender cm (ssp, qsp) r = do
  let (srs, qrs) = split r

  unless (null srs) $ renderSprites cm ssp srs
  unless (null qrs) $ renderQuads   cm qsp qrs
  where split :: Render -> ([Render], [Render])
        split sr@(SpriteRender _ _ _) = ([sr],   [])
        split qr@(QuadRender   _ _ _) = (  [], [qr])
        split    (Renders          l) =
          let l' = map split l in
            if null l'
              then ([], [])
              else foldl1 joinTuple l'

        joinTuple :: ([Render], [Render]) -> ([Render], [Render]) -> ([Render], [Render])
        joinTuple (srs1, qrs1) (srs2, qrs2) = (srs1 ++ srs2, qrs1 ++ qrs2)
