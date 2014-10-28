-- | This module handles creating an @'Entity'@ that represents the background.
module WhatIsThisGame.Controllers.Background where

--------------------
-- Global Imports --
import FRP.Elerea.Param
import Control.Monad
import Control.Lens
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
         , getSize     = V2   100 100
         , getHealth   = 0
         , shouldShoot = False
         }

-- | The controller that generates the transform each update.
backgroundController :: Signal World -> SignalGen Float (Signal EntityTransform)
backgroundController _ = return $ return id
{-
  ssize <- renderSize
  return $ liftM (\size -> \e -> e { getSize = size & _x .~ (size ^. _x * 100) }) ssize
-}

-- | The front-end for the background.
background :: Signal World -> SignalGen Float (Signal Entity)
background w = backgroundController w >>= entity initialBackground
