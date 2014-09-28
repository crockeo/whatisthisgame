-- | The module that contains all of the type definitions in the library.
--   They're contained in one place so that recursive module imports may be
--   avoided when only because of type definitions.
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module WhatIsThisGame.Data where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL
import Data.Vinyl.Universe
import Linear.Matrix
import Data.Vinyl
import Linear.V2

----------
-- Code --

-- | The matrix of the camera.
type CamMatrix = PlainFieldRec '["cam" ::: M33 GLfloat]

-- | A vertex coordinate to pass to GLSL.
type VertexCoord  = "vertexCoord"  ::: V2 GLfloat

-- | A texture coordinate to pass to GLSL.
type TextureCoord = "textureCoord" ::: V2 GLfloat

-- | An instance of the @'VertexCoord'@.
vertexCoord :: SField VertexCoord
vertexCoord = SField

-- | An instance of the @'TextureCoord'@.
textureCoord :: SField TextureCoord
textureCoord = SField

-- | Specifying that a type can be rendered.
class Renderable a where
  render :: a -> IO ()

-- | A pure form of rendering sprites - can be optimized pre-render to increase
--   performance.
data SpriteRender = SpriteRender TextureObject (V2 Float) (V2 Float)
                  | SpriteRenders [SpriteRender]

-- | Specifying the @'World'@ type.
data World = World
