-- | This module provides the logic for constructing and rendering the game
--   world.
module WhatIsThisGame.World where

--------------------
-- Global Imports --
import FRP.Elerea.Param
import Data.Monoid
import Linear.V2

-------------------
-- Local Imports --
import WhatIsThisGame.Data

----------
-- Code --

-- | Providing the rendering for a @'World'@.
instance Renderable World where
  render assets _ =
    mconcat [ SpriteRender (getSprites assets ! "res/test.png")
                           (V2 0.5 0.5)
                           (V2 3   3  )
            ]

-- | Providing an always-updated @'World'@.
world :: SignalGen Float (Signal World)
world = return $ return World
