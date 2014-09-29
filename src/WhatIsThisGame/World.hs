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
    renderQuads cm
                (getShaders assets ! "res/color")
                [white, red, green, blue]
                [ (V2 0.25 0.25, V2 0.5 0.5)
                , (V2 1    1   , V2 0.8 0.8)
                , (V2 2    2   , V2 1.6 1.6)
                ]

-- | Providing an always-updated @'World'@.
world :: SignalGen Float (Signal World)
world = return $ return World
