-- | This module provides the logic for constructing and rendering the game
--   world.
module WhatIsThisGame.World where

--------------------
-- Global Imports --
import Control.Applicative
import FRP.Elerea.Param
import Data.Monoid
import Linear.V2

-------------------
-- Local Imports --
import WhatIsThisGame.EntityUpdate
import WhatIsThisGame.Entity
import WhatIsThisGame.Data

----------
-- Code --

-- | Providing the rendering for a @'World'@.
instance Renderable World where
  render assets (World es) =
    mconcat $ map (render assets) es

-- | REMOVE LATER
--   A placeholder for the initial player definition.
initialPlayer :: Entity
initialPlayer = Entity { getName     = "res/player.png"
                       , getPosition = V2 0.1 1
                       , getSize     = V2 0.4 1
                       , getHealth   = 150
                       , shouldShoot = False
                       }

-- | Providing an always-updated @'World'@.
world :: SignalGen Float (Signal World)
world = do
  pc <- playerController

  ents <- entities [(initialPlayer, pc)]
  return $ World <$> ents
