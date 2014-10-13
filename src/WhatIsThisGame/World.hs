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
import WhatIsThisGame.Controllers.Background
import WhatIsThisGame.Controllers.Bullet
import WhatIsThisGame.Controllers.Player
import WhatIsThisGame.Input
import WhatIsThisGame.Data

----------
-- Code --

-- | Providing the rendering for a @'World'@.
instance Renderable World where
  render assets w =
     mconcat $ mconcat [ map (render assets) $ worldGetBackgrounds w
                       , map (render assets) $ worldGetEnemies w
                       , map (render assets) $ worldGetBullets w
                       , [render assets $ worldGetPlayer w]
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
  p   <- player y w
  bus <- bullets (pure PlayerBullet) (fmap getPosition p) (fmap shouldShoot p)

  delay (initialWorld $ initialPlayer y) $ World <$> p
                                                 <*> sequence [b]
                                                 <*> pure []
                                                 <*> bus
  where calcPos :: V2 Float -> Float
        calcPos (V2 _ h) = (h / 2) - (playerSize ^. _y / 2)

-- | Providing an always-updated @'World'@.
world :: SignalGen Float (Signal World)
world = mfix world'
