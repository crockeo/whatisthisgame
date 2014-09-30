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

-- | The back-end for updating an @'Entity'@.
entity' :: Entity -> Signal EntityTransform -> Signal Entity -> SignalGen Float (Signal Entity)
entity' e set se = do
  delay e $ set <*> se

-- | The front-end for updating an @'Entity'@.
entity :: Entity -> Signal EntityTransform -> SignalGen Float (Signal Entity)
entity e set = mfix $ entity' e set

-- | Creating a number of entities.
entities :: [(Entity, Signal EntityTransform)] -> SignalGen Float (Signal [Entity])
entities eps = sequence <$> mapM (uncurry entity) eps
