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

-- | Joining a bunch of @'SpriteBatch'@es contained in a @'Renders'@. NOTE: It
--   automatically assumes that each @'Sprite'@ is THE SAME.
joinSpriteBatches :: Renders -> SpriteBatch
joinSpriteBatches =
  foldl joinSpriteBatch (SpriteBatch [])
  where joinSpriteBatch :: SpriteBatch -> SpriteBatch -> SpriteBatch
        joinSpriteBatch (SpriteBatch rs1) (SpriteBatch rs2) =
          SpriteBatch $ rs1 ++ rs2

-- | Making the final @'SpriteBatch'@ for a list of @'Renderable'@s.
makeSpriteBatch :: Renderable a => Assets -> [a] -> SpriteBatch
makeSpriteBatch assets = joinSpriteBatches . foldl (++) [] . map (render assets)

-- | Providing the rendering for the @'World'@.
instance Renderable World where
  render assets w =
    [ makeSpriteBatch assets $ worldGetBackgrounds w
    , makeSpriteBatch assets $ worldGetAsteroids   w
    , makeSpriteBatch assets $ worldGetEnemies     w
    , makeSpriteBatch assets $ worldGetBullets     w
    , head $ render assets $ worldGetPlayer w
    ]

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
  t   <- periodically 0.25 $ fmap shouldShoot p
  bus <- bullets w t (pure PlayerBullet) (fmap getPosition p) (fmap getSize p)
  s   <- currentScore
  ls  <- calculateLives $ fmap snd esd

  delay (initialWorld $ initialPlayer y) $ World <$> p
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
