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
    ( mconcat [ QuadRender [white, red, green, blue]
                           (V2 0.25 0.25)
                           (V2 0.5  0.5 )
              , QuadRender [white, red, green, blue]
                           (V2 1    1   )
                           (V2 0.8  0.8 )
              , QuadRender [white, red, green, blue]
                           (V2 2    2   )
                           (V2 1.6  1.6 )
              ]
    , getShaders assets ! "res/color"
    )

-- | Providing an always-updated @'World'@.
world :: SignalGen Float (Signal World)
world = return $ return World
