-- | This module handles creating an @'Entity'@ that represents the background.
module WhatIsThisGame.Controllers.Background where

--------------------
-- Global Imports --
import Prelude hiding ((.))
import Control.Monad
import Control.Lens
import Control.Wire
import Linear.V2

-------------------
-- Local Imports --
import WhatIsThisGame.Entity
import WhatIsThisGame.Input
import WhatIsThisGame.Data

----------
-- Code --

-- | The initial state of the background
initialBackground :: Entity
initialBackground =
  Entity { getName     = "res/background.png"
         , getPosition = V2   0 0
         , getSize     = V2   0 0
         , getHealth   = 0
         , shouldShoot = False
         }

-- | The controller that generates the transform each update.
backgroundController :: Wire s e IO World EntityTransform
backgroundController =
  backgroundController' . renderSize
  where backgroundController' :: Wire s e IO (V2 Float) EntityTransform
        backgroundController' =
          mkSF_ $ \size ->
            \e -> e { getSize = size & _x .~ (size ^. _x * 100) }

-- | The front-end for the background.
background :: Wire s e IO World Entity
background = entity initialBackground . backgroundController
