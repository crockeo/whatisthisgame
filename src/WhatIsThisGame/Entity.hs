-- | This module provides some generalized wires to run entites within the
--   Elerea network.
module WhatIsThisGame.Entity (entity) where

--------------------
-- Global Imports --
import Control.Wire

-------------------
-- Local Imports --
import WhatIsThisGame.Data

----------
-- Code --

-- | The wire for updating an @'Entity'@.
entity :: Entity -> Wire s e IO EntityTransform Entity
entity e =
  mkSFN $ \et ->
    let e' = et e in
      e' `seq` (e, entity e')
