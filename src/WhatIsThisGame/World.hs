-- | This module provides the logic for constructing and rendering the game
--   world.
module WhatIsThisGame.World where

--------------------
-- Global Imports --
import FRP.Elerea.Param
import Linear.V2

-------------------
-- Local Imports --
import WhatIsThisGame.Rendering
import WhatIsThisGame.Data

----------
-- Code --

-- | Providing the rendering for a @'World'@.
instance Renderable World where
  render cm assets World =
    renderQuads cm (getShaders assets ! "res/color") $
      [ ( V2 0.25 0.25
        , V2 0.5  0.5
        )
      ]

-- | Providing an always-updated @'World'@.
world :: SignalGen Float (Signal World)
world = return $ return World
