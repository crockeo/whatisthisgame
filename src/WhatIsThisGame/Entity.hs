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

-------------------
-- Local Imports --
import WhatIsThisGame.Data

----------
-- Code --

-- | The back-end for updating an @'Entity'@.
entity' :: Entity -> Signal EntityUpdate -> Signal Entity -> SignalGen Float (Signal Entity)
entity' e _ _ = do
  delay e $ pure e

-- | The front-end for updating an @'Entity'@.
entity :: Entity -> Signal EntityUpdate -> SignalGen Float (Signal Entity)
entity e seu = mfix $ entity' e seu

-- | Creating a number of entities.
entities :: [(Entity, Signal EntityUpdate)] -> SignalGen Float (Signal [Entity])
entities eps = sequence <$> mapM (uncurry entity) eps