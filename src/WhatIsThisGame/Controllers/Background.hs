-- | This module handles creating an @'Entity'@ that represents the background.
module WhatIsThisGame.Controllers.Background (backgrounds) where

--------------------
-- Global Imports --
import FRP.Elerea.Param
import Data.Maybe
import Linear.V2

-------------------
-- Local Imports --
import WhatIsThisGame.Entity
import WhatIsThisGame.Utils
import WhatIsThisGame.Data

----------
-- Code --

-- | Changing the size of the background.
backgroundSize :: V2 Float
backgroundSize = 100

-- | Creating a new background.
newBackground :: V2 Float -> Entity
newBackground pos =
  Entity { getName     = "res/background.png"
         , getPosition = pos
         , getSize     = backgroundSize
         , getHealth   = 0
         , shouldShoot = False
         }

-- | A list of initial, tiled backgrounds.
initialBackgrounds :: [Entity]
initialBackgrounds =
  [ newBackground $ V2   0   0
  , newBackground $ V2 100   0
  , newBackground $ V2   0 100
  , newBackground $ V2 100 100
  , newBackground $ V2 200   0
  , newBackground $ V2 200 100
  ]

-- | The controller for updating the background.
backgroundsController :: Signal World -> SignalGen Float ([Signal EntityTransform])
backgroundsController _ = return $ repeat $ return return

-- | The list of @'Maybe'@ backgrounds. Just used to satisfy the type
--   constraint.
maybeBackgrounds :: Signal World -> SignalGen Float (Signal [Maybe Entity])
maybeBackgrounds w = backgroundsController w >>= (entities . zip initialBackgrounds)

-- | The list of backgrounds with the @'Maybe'@s removed.
backgrounds :: Signal World -> SignalGen Float (Signal [Entity])
backgrounds = sgMap catMaybes . maybeBackgrounds
