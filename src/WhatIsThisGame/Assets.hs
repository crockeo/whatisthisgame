-- | This module provides the ability to load assets from the filesystem.
module WhatIsThisGame.Assets (loadAssets) where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL hiding (Shader)
import Graphics.GLUtil hiding (loadShader)
import qualified Data.Map.Strict as Map
import Graphics.Rendering.FTGL
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

-- | The raw backend for loading a @'Font'@.
loadFont :: FilePath -> IO Font
loadFont = createOutlineFont

-- | Performing a single asset load.
performLoad :: AssetLoad -> IO Assets
performLoad (SpriteLoad fp) = liftM (\sprite -> mempty { getSprites = Map.fromList [(fp, sprite)] }) $ loadSprite fp
performLoad (ShaderLoad fp) = liftM (\shader -> mempty { getShaders = Map.fromList [(fp, shader)] }) $ loadShader fp
performLoad (FontLoad   fp) = liftM (\font   -> mempty { getFonts   = Map.fromList [(fp, font  )] }) $ loadFont   fp
performLoad (AssetLoads l ) = liftM mconcat $ mapM performLoad l

-- | Generating a lod string for a number of frames.
generateLoadFrames :: Int -> String -> [AssetLoad]
generateLoadFrames cap prefix =
  [SpriteLoad $ prefix ++ formatString n ++ ".png" | n <- [1 .. cap]]
  where formatString :: Int -> String
        formatString n
          | n < 10    = "0" ++ show n
          | otherwise =        show n

-- | The list of assets I require.
loadAssets :: IO Assets
loadAssets =
  performLoad $ mconcat $
    [ ShaderLoad "res/game2d"
    
    , SpriteLoad "res/background.png"
    , SpriteLoad "res/bullet.png"
    
    , FontLoad "res/speculo.ttf"
    , FontLoad "res/speculum.ttf"
    ] ++
    
    generateLoadFrames  4 "res/player/"      ++
    generateLoadFrames  3 "res/enemy/"       ++
    generateLoadFrames 15 "res/asteroid/01/"
