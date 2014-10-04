-- | This module provides the ability to load assets from the filesystem.
module WhatIsThisGame.Assets (loadAssets) where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL hiding (Shader)
import qualified Data.Map.Strict as Map
import Graphics.GLUtil hiding (loadShader)
import Control.Monad
import Data.Monoid

-------------------
-- Local Imports --
import WhatIsThisGame.Data

----------
-- Code --

-- | The raw backend for loading a @'Sprite'@.
loadSprite :: FilePath -> IO Sprite
loadSprite fp = do
  img <- fmap (either error id) $ readTexture fp

  textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
  texture2DWrap           $= (Repeated, ClampToEdge)

  return $ Sprite img

-- | The raw backend for loading a @'ShaderProgram'@.
loadShader :: FilePath -> IO Shader
loadShader = liftM Shader . loadShaderFamily

-- | Performing a single asset load.
performLoad :: AssetLoad -> IO Assets
performLoad (SpriteLoad fp) = liftM (\sprite -> mempty { getSprites = Map.fromList [(fp, sprite)] }) $ loadSprite fp
performLoad (ShaderLoad fp) = liftM (\shader -> mempty { getShaders = Map.fromList [(fp, shader)] }) $ loadShader fp
performLoad (AssetLoads l ) = liftM mconcat $ mapM performLoad l

-- | The list of assets I require.
loadAssets :: IO Assets
loadAssets =
  performLoad $ mconcat
    [ ShaderLoad "res/game2d"
    , ShaderLoad "res/color"

    , SpriteLoad "res/player/01.png"
    , SpriteLoad "res/player/02.png"
    , SpriteLoad "res/player/03.png"
    , SpriteLoad "res/player/04.png"

    , SpriteLoad "res/background.png"
    , SpriteLoad "res/bullet.png"
    ]
