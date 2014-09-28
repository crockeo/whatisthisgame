-- | The module that contains all of the type definitions in the library.
--   They're contained in one place so that recursive module imports may be
--   avoided when only because of type definitions.
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module WhatIsThisGame.Data where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL hiding ( Shader
                                        , Color
                                        )

import qualified Data.Map.Strict as Map
import Data.Vinyl.Universe
import Graphics.GLUtil
import Linear.Matrix
import Data.Monoid
import Data.Vinyl
import Linear.V4
import Linear.V2

----------
-- Code --

-- | The matrix of the camera.
type CamMatrix = PlainFieldRec '["cam" ::: M33 GLfloat]

-- | A vertex coordinate to pass to GLSL.
type VertexCoord  = "vertexCoord"  ::: V2 GLfloat

-- | A texture coordinate to pass to GLSL.
type TextureCoord = "textureCoord" ::: V2 GLfloat

-- | A color to pass to GLSL.
type GLSLColor = "color" ::: V4 GLfloat

-- | An instance of the @'VertexCoord'@.
vertexCoord :: SField VertexCoord
vertexCoord = SField

-- | An instance of the @'TextureCoord'@.
textureCoord :: SField TextureCoord
textureCoord = SField

-- | An instance for the @'GLSLColor'@.
glslColor :: SField GLSLColor
glslColor = SField

-- | A synonym for a color represented by 4 @'Float'@s.
newtype Color = Color (V4 Float)

-- | An instance of the @'Color'@ datatype.
white, black, red, green, blue :: Color
white = Color $ V4 1 1 1 1
black = Color $ V4 0 0 0 1
red   = Color $ V4 1 0 0 1
green = Color $ V4 0 1 0 1
blue  = Color $ V4 0 0 1 1

-- | A data structure to represent a sprite.
newtype Sprite = Sprite TextureObject

-- | A data structure to represent a shader.
newtype Shader = Shader ShaderProgram

-- | An abstract representation of a request to load an asset.
data AssetLoad = SpriteLoad FilePath
               | ShaderLoad FilePath
               | AssetLoads [AssetLoad]

-- | Allowing @'AssetLoad'@s to be joined together into one nebulous
--   @'AssetLoad'@ containing all of the loads.
instance Monoid AssetLoad where
  mempty = AssetLoads []

  mappend (AssetLoads l1) (AssetLoads l2) = AssetLoads $ l1 ++ l2
  mappend (AssetLoads l1) assetLoad       = AssetLoads $ assetLoad : l1
  mappend assetLoad       (AssetLoads l1) = AssetLoads $ assetLoad : l1
  mappend assetLoad1      assetLoad2      = AssetLoads [assetLoad1, assetLoad2]

-- | An API for accessing assets.
data Assets = Assets { getSprites :: Map.Map FilePath Sprite
                     , getShaders :: Map.Map FilePath Shader
                     }

-- | Allowing different @'Assets'@ to be joined. Primarily for easier loading
--   of assets.
instance Monoid Assets where
  mempty = Assets { getSprites = mempty
                  , getShaders = mempty
                  }

  mappend (Assets sp1 sh1) (Assets sp2 sh2) =
    Assets { getSprites = sp1 `mappend` sp2
           , getShaders = sh1 `mappend` sh2
           }

-- | A synonym for map's access function.
(!) :: Ord a => Map.Map a b -> a -> b
(!) = (Map.!)

-- | Specifying that a type can be rendered.
class Renderable a where
  render :: CamMatrix -> Assets -> a -> IO ()

-- | A pure form of rendering sprites - can be optimized pre-render to increase
--   performance.
data SpriteRender = SpriteRender Sprite (V2 Float) (V2 Float)
                  | SpriteRenders [SpriteRender]

-- | Specifying the @'World'@ type.
data World = World
