-- | This module provides the logic for constructing and rendering the game
--   world.
module WhatIsThisGame.World where

--------------------
-- Global Imports --
import Control.Applicative
import Control.Monad.Fix
import FRP.Elerea.Param
import Control.Lens
import Data.Maybe
import Linear.V2

-------------------
-- Local Imports --
import WhatIsThisGame.Controllers.EnemySpawner
import WhatIsThisGame.Controllers.Background
import WhatIsThisGame.Controllers.Asteroids
import WhatIsThisGame.Controllers.Bullet
import WhatIsThisGame.Controllers.Player
import WhatIsThisGame.Controllers.Lives
import WhatIsThisGame.Input
import WhatIsThisGame.Utils
import WhatIsThisGame.Data

----------
-- Code --

-- | Prepending a @'SpriteRender'@ to a @'SpriteBatch'@.
prependSpriteRender :: SpriteRender -> SpriteBatch -> SpriteBatch
prependSpriteRender sr (SpriteBatch ss) = SpriteBatch $ sr : ss

-- | Joining a @'[Renders]'@ (which should represent a bunch of single
--   @'RenderSprite'@ calls) into a single @'Render'@ that contains a
--   @'RenderSprites'@.
makeRenderSprites :: Renderable a => Assets -> [a] -> Render
makeRenderSprites assets =
  foldl join (RenderSprites $ SpriteBatch []). foldl (++) [] . map (render assets)
  where join :: Render -> Render -> Render
        join (RenderSprite  s1) (RenderSprite  s2) = RenderSprites $ SpriteBatch [s1, s2]
        join (RenderSprite  sr) (RenderSprites ss) = RenderSprites $ prependSpriteRender sr ss
        join (RenderSprites ss) (RenderSprite  sr) = RenderSprites $ prependSpriteRender sr ss
        join                 _                  _  = error "This should not be happening."

-- | Rendering while the game is going on.
midRender :: Assets -> World -> Renders
midRender assets w =
  [ makeRenderSprites assets $ worldGetBackgrounds w
  , makeRenderSprites assets $ worldGetAsteroids   w

  , RenderText $ TextRender (getFonts assets ! "res/speculum.ttf")
                            ("Score: " ++ show (worldGetScore w))
                            (V2 5 5)
                            4

  , RenderText $ TextRender (getFonts assets ! "res/speculum.ttf")
                            ("Lives: " ++ show (worldGetLives w))
                            (V2 5 15)
                            4

  , makeRenderSprites assets $ worldGetEnemies     w
  , makeRenderSprites assets $ worldGetBullets     w
  , head $ render assets     $ worldGetPlayer      w
  ]

-- | Rendering when the player has lost.
lostRender :: Assets -> World -> Renders
lostRender assets w =
  [ makeRenderSprites assets $ worldGetBackgrounds w
  , makeRenderSprites assets $ worldGetAsteroids   w
  , makeRenderSprites assets $ worldGetEnemies     w
  , makeRenderSprites assets $ worldGetBullets     w
  , head $ render assets     $ worldGetPlayer      w

  , RenderText $ TextRender (getFonts assets ! "res/speculum.ttf")
                            ("You lost! (With a score of " ++ show (worldGetScore w) ++ "!)")
                            (V2 5 15)
                            25
  ]

-- | Providing the rendering for the @'World'@.
instance Renderable World where
  render assets w =
    if worldGetLives w <= 0
      then lostRender assets w
      else midRender  assets w

-- | The initial state of the world.
initialWorld :: Entity -> World
initialWorld p =
  World { worldGetPlayer      = p
        , worldGetBackgrounds = []
        , worldGetAsteroids   = []
        , worldGetEnemies     = []
        , worldGetBullets     = []
        , worldGetScore       = 0
        , worldGetLives       = initialLives
        }

-- | Providing the back-end to the @'world'@ function.
world' :: Signal World -> SignalGen Float (Signal World)
world' w = do
  y <- renderSize >>= (fmap calcPos . snapshot)

  bs  <- backgrounds w
  as  <- asteroids w
  esd <- enemies w
  p   <- sgMap fromJust $ player y w
  t   <- periodically 0.5 $ fmap shouldShoot p
  bus <- bullets w t (pure PlayerBullet) (fmap getPosition p) (fmap getSize p)
  s   <- currentScore
  ls  <- calculateLives $ fmap snd esd

  delay (initialWorld $ initialPlayer y) $
    (>>=) w $ \w' ->
      if worldGetLives w' <= 0
        then w
        else World <$> p
                   <*> bs
                   <*> as
                   <*> fmap fst esd
                   <*> bus
                   <*> s
                   <*> ls
  where calcPos :: V2 Float -> Float
        calcPos (V2 _ h) = (h / 2) - (playerSize ^. _y / 2)

-- | Providing an always-updated @'World'@.
world :: SignalGen Float (Signal World)
world = mfix world'
