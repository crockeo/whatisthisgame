-- | This module provides the logic for constructing and rendering the game
--   world.
module WhatIsThisGame.World where

--------------------
-- Global Imports --
import Control.Applicative
import Control.Monad.Fix
import FRP.Elerea.Param
import Control.Lens
import Data.Monoid
import Linear.V2

-------------------
-- Local Imports --
import WhatIsThisGame.Controllers.EnemySpawner
import WhatIsThisGame.Controllers.Background
import WhatIsThisGame.Controllers.Bullet
import WhatIsThisGame.Controllers.Player
import WhatIsThisGame.Input
import WhatIsThisGame.Utils
import WhatIsThisGame.Data

----------
-- Code --

-- | Joining a bunch of @'SpriteBatch'@es contained in a @'Renders'@.
joinSpriteBatches :: Renders -> SpriteBatch
joinSpriteBatches =
  foldl joinSpriteBatch (SpriteBatch [])
  where joinSpriteBatch :: SpriteBatch -> SpriteBatch -> SpriteBatch
        joinSpriteBatch (SpriteBatch rs1) (SpriteBatch rs2) =
          SpriteBatch $ rs1 ++ rs2

-- | Providing the rendering for the @'World'@.
instance Renderable World where
  render assets w =
    [ joinSpriteBatches $ foldl (++) [] $ map (render assets) $ worldGetBackgrounds w
    , joinSpriteBatches $ foldl (++) [] $ map (render assets) $ worldGetEnemies     w
    , joinSpriteBatches $ foldl (++) [] $ map (render assets) $ worldGetBullets     w
    , head $ render assets $ worldGetPlayer w
    ]

-- | The initial state of the world.
initialWorld :: Entity -> World
initialWorld p =
  World { worldGetPlayer      = p
        , worldGetBackgrounds = []
        , worldGetEnemies     = []
        , worldGetBullets     = []
        }

-- | Providing the back-end to the @'world'@ function.
world' :: Signal World -> SignalGen Float (Signal World)
world' w = do
  y <- renderSize >>= (fmap calcPos . snapshot)

  b   <- background w
  es  <- enemies w
  p   <- player y w
  t   <- periodically 0.25 $ fmap shouldShoot p
  bus <- bullets t (pure PlayerBullet) (fmap getPosition p) (fmap getSize p)

  delay (initialWorld $ initialPlayer y) $ World <$> p
                                                 <*> sequence [b]
                                                 <*> es
                                                 <*> bus
  where calcPos :: V2 Float -> Float
        calcPos (V2 _ h) = (h / 2) - (playerSize ^. _y / 2)

-- | Providing an always-updated @'World'@.
world :: SignalGen Float (Signal World)
world = mfix world'
