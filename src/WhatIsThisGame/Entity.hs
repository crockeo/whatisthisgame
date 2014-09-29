-- | This module provides some generalized wires to run entites within the
--   Elerea network.
module WhatIsThisGame.Entity ( entity
                             , entities
                             ) where

--------------------
-- Global Imports --
import Control.Applicative
import Control.Monad.Fix
import FRP.Elerea.Param
import Control.Lens
import Linear.V2

-------------------
-- Local Imports --
import WhatIsThisGame.Data

----------
-- Code --

-- | Updating an @'Entity'@ with an @'EntityUpdate'@.
update :: Float -> EntityUpdate -> Entity -> Entity
update dt eu e = e { getPosition = getPosition e & _x .~ getPosition e ^. _x + euMove eu * dt }

-- | The back-end for updating an @'Entity'@.
entity' :: Entity -> Signal EntityUpdate -> SignalGen Float (Signal Entity)
entity' e eu =
  transfer e update eu

-- | The front-end for updating an @'Entity'@.
entity :: Entity -> Signal EntityUpdate -> SignalGen Float (Signal Entity)
entity e seu = entity' e seu

-- | Creating a number of entities.
entities :: [(Entity, Signal EntityUpdate)] -> SignalGen Float (Signal [Entity])
entities eps = sequence <$> mapM (uncurry entity) eps