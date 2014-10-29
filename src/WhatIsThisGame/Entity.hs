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
entity' :: Entity -> Signal EntityTransform -> Signal (Maybe Entity) -> SignalGen Float (Signal (Maybe Entity))
entity' e set sme =
  delay (Just e) $ do
    et <- set
    me <- sme
    return $ me >>= et

-- | The front-end for updating an @'Entity'@.
entity :: Entity -> Signal EntityTransform -> SignalGen Float (Signal (Maybe Entity))
entity e set = mfix $ entity' e set

-- | Creating a number of entities.
entities :: [(Entity, Signal EntityTransform)] -> SignalGen Float (Signal [Maybe Entity])
entities eps = sequence <$> mapM (uncurry entity) eps
