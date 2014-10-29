-- | The module that contains all of the type definitions in the library.
--   They're contained in one place so that recursive module imports may be
--   avoided when only because of type definitions.
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module WhatIsThisGame.Data where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL hiding ( Shader
                                        , Color
                                        )

import qualified Data.Map.Strict as Map
import Graphics.UI.GLFW as GLFW
import Data.Vinyl.Universe
import Graphics.GLUtil
import Linear.Matrix
import Control.Lens
import Data.Monoid
import Data.Vinyl
import Linear.V4
import Linear.V2

----------
-- Code --

-- | The data structure for window configuration.
data WindowConfig = WindowConfig { _cfgWidth      :: Int
                                 , _cfgHeight     :: Int
                                 , _cfgFullscreen :: Bool
                                 , _cfgJumpKey    :: Key
                                 , _cfgShootKey   :: Key
                                 , _cfgSlowKey    :: Key
                                 , _cfgFastKey    :: Key
                                 }

-- | Showing the @'WindowConfig'@ as a @'String'@.
instance Show WindowConfig where
  show wc = mconcat [      "width=", show $                 _cfgWidth wc, "\n"
                    ,     "height=", show $                _cfgHeight wc, "\n"
                    , "fullscreen=", show $            _cfgFullscreen wc, "\n"
                    ,    "jumpkey=", show $ fromEnum $    _cfgJumpKey wc, "\n"
                    ,   "shootkey=", show $ fromEnum $   _cfgShootKey wc, "\n"
                    ,    "slowkey=", show $ fromEnum $    _cfgSlowKey wc, "\n"
                    ,    "fastkey=", show $ fromEnum $    _cfgFastKey wc, "\n"
                    ]

$(makeLenses ''WindowConfig)

-- | The default @'WindowConfig'@.
defaultWindowConfig :: WindowConfig
defaultWindowConfig = WindowConfig { _cfgWidth      = 640
                                   , _cfgHeight     = 480
                                   , _cfgFullscreen = False
                                   , _cfgJumpKey    = CharKey ' '
                                   , _cfgShootKey   = CharKey 'E'
                                   , _cfgSlowKey    = CharKey 'A'
                                   , _cfgFastKey    = CharKey 'D'
                                   }

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

-- | A type to be used when calculating collision.
data CollisionRectangle = CollisionRectangle { _crPos  :: V2 Float
                          , _crSize :: V2 Float
                          } 
                         
-- | This instance defines that a type can be converted to a
--   @'CollisionRectangle'@, and can therefore be used to calculate collision.
class Collidable a where
  toCollisionRectangle :: a -> CollisionRectangle

-- | A data structure to represent a sprite.
newtype Sprite = Sprite TextureObject

-- | A data structure to represent an animation. In other words, a list of
--   @'Sprite'@s.
newtype Animation = Animation [String]

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

-- | A pure form of render calls. Can be optimized pre-render to increase
--   performance.
data Render = SpriteRender Sprite (V2 Float) (V2 Float)

-- | A type to represent a batch of @'Sprite's@ to render. They should ALL have
--   the same @'Sprite'@.
data SpriteBatch = SpriteBatch [Render]

-- | A type to represent multiple @'Render'@ calls.
type Renders = [SpriteBatch]

-- | A synonym for map's access function.
(!) :: Ord a => Map.Map a b -> a -> b
(!) = (Map.!)

-- | Specifying that a type can be rendered.
class Renderable a where
  render :: Assets -> a -> Renders

-- | The definition of the information necessary for an entity.
data Entity = Entity { getName        :: String
                     , getPosition    :: V2 Float
                     , getSize        :: V2 Float
                     , getHealth      :: Float
                     , shouldShoot    :: Bool
                     }

-- | Rendering an entity.
instance Renderable Entity where
  render assets e =
    [ SpriteBatch [ SpriteRender (getSprites assets ! getName e)
                                 (getPosition e)
                                 (getSize     e)
                  ]
    ]
    
-- | Allowing an @'Entity'@ to check collision.
instance Collidable Entity where
  toCollisionRectangle entity =
    CollisionRectangle { _crPos  = getPosition entity
                       , _crSize = getSize     entity
                       }

-- | A type to represent the defaults for a type of bullet.
data BulletType = PlayerBullet
                | EnemyBullet

-- | The bullet type.
data Bullet = Bullet { getBulletType     :: BulletType
                     , getBulletPosition :: V2 Float
                     , getBulletSize     :: V2 Float
                     , getBulletDamage   :: V2 Float
                     , getBulletSpeed    :: V2 Float
                     }

-- | Rendering a bullet.
instance Renderable Bullet where
  render assets b =
    [ SpriteBatch [ SpriteRender (getSprites assets ! getSpriteName b)
                                 (getBulletPosition b)
                                 (getBulletSize b)
                  ]
    ]
    where getSpriteName :: Bullet -> String
          getSpriteName bt =
            case getBulletType bt of
              PlayerBullet -> "res/bullet.png"
              EnemyBullet  -> "res/bullet.png"

-- | Allowing a @'Bullet'@ to check collision.
instance Collidable Bullet where
  toCollisionRectangle bullet =
    CollisionRectangle { _crPos  = getBulletPosition bullet
                       , _crSize = getBulletSize     bullet
                       }

-- | The default move speed of the player.
playerMoveSpeed :: Float
playerMoveSpeed = 20

-- | The default height of the ground.
groundHeight :: Float
groundHeight = 10

-- | Checking if an @'Entity'@ is dead.
isDead :: Entity -> Bool
isDead e = getHealth e <= 0

-- | Checking if an @'Entity'@ is on the ground.
onGround :: Entity -> Bool
onGround e = (getPosition e ^. _y) <= groundHeight

-- | An alternative type to be used instead of the @'EntityUpdate'@ type. It
--   works by creating one through an @'Entity'@ controller and then applying
--   it to an @'Entity'@ to create the next @'Entity'@ for the frame.
type EntityTransform = (Entity -> Maybe Entity)

-- | Specifying the @'World'@ type.
data World = World { worldGetPlayer      :: Entity
                   , worldGetBackgrounds :: [Entity]
                   , worldGetAsteroids   :: [Entity]
                   , worldGetEnemies     :: [Entity]
                   , worldGetBullets     :: [Bullet]
                   , worldGetScore       :: Int
                   }
