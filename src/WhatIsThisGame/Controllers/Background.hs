-- | This module handles creating an @'Entity'@ that represents the background.
module WhatIsThisGame.Controllers.Background (backgrounds) where

--------------------
-- Global Imports --
import FRP.Elerea.Param
import Linear.V2

-------------------
-- Local Imports --
import WhatIsThisGame.Entity
import WhatIsThisGame.Data

----------
-- Code --

backgroundSize :: V2 Float
backgroundSize = 100

newBackground :: V2 Float -> Entity
newBackground pos =
  Entity { getName     = "res/background.png"
         , getPosition = pos
         , getSize     = backgroundSize
         , getHealth   = 0
         , shouldShoot = False
         }

initialBackgrounds :: [Entity]
initialBackgrounds =
  [ newBackground $ V2   0   0
  , newBackground $ V2 100   0
  , newBackground $ V2   0 100
  , newBackground $ V2 100 100
  , newBackground $ V2 200   0
  , newBackground $ V2 200 100
  ]

backgroundsController :: Signal World -> SignalGen Float ([Signal EntityTransform])
backgroundsController _ = return $ repeat $ return id

backgrounds :: Signal World -> SignalGen Float (Signal [Entity])
backgrounds w = backgroundsController w >>= (entities . zip initialBackgrounds)
