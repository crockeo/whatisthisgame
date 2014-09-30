-- | This module provides the logic for constructing and rendering the game
--   world.
module WhatIsThisGame.World where

--------------------
-- Global Imports --
import Control.Applicative
import Control.Monad.Fix
import FRP.Elerea.Param
import Data.Monoid

-------------------
-- Local Imports --
import WhatIsThisGame.Controllers.Player
import WhatIsThisGame.Data

----------
-- Code --

-- | Providing the rendering for a @'World'@.
instance Renderable World where
  render assets (World es) =
    mconcat $ map (render assets) es

-- | The initial state of the world.
initialWorld :: World
initialWorld = World []

-- | Providing the back-end to the @'world'@ function.
world' :: Signal World -> SignalGen Float (Signal World)
world' w = do
  p <- player w
  delay initialWorld $ (\a -> World [a]) <$> p

-- | Providing an always-updated @'World'@.
world :: SignalGen Float (Signal World)
world = mfix world'
