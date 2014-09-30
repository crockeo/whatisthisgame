-- | This module provides a controller for running an entity to represent a
--   player within the game world.
module WhatIsThisGame.Controllers.Player where

--------------------
-- Global Imports --
import Graphics.UI.GLFW as GLFW
import Control.Applicative
import FRP.Elerea.Param
import Control.Lens
import Linear.V2

-------------------
-- Local Imports --
import WhatIsThisGame.Entity
import WhatIsThisGame.Input
import WhatIsThisGame.Data

----------
-- Code --

-- | Calculating the speed of the player.
calcSpeed :: Bool -> Bool -> Float
calcSpeed False False = playerMoveSpeed
calcSpeed  True  True = playerMoveSpeed
calcSpeed False  True = playerMoveSpeed + playerMoveSpeed / 2
calcSpeed  True False = playerMoveSpeed - playerMoveSpeed / 2

-- | Constructing the function to transform an @'Entity'@.
makeUpdate :: Float -> Bool -> Bool -> Float -> a -> EntityTransform
makeUpdate dt _ ss sp _ =
  \e -> e { getPosition = getPosition e & _x .~ getPosition e ^. _x + sp * dt
          , shouldShoot = ss
          }

-- | The initial state of the player.
initialPlayer :: Entity
initialPlayer =
  Entity { getName     = "res/player.png"
         , getPosition = V2 5 10
         , getSize     = V2 20 30
         , getHealth   = 150
         , shouldShoot = False
         }

-- | An alternate version of the @'playerController'@.
playerController :: Signal World -> SignalGen Float (Signal EntityTransform)
playerController _ = do
  jkd <- keyDown (CharKey ' ')
  skd <- keyDown (CharKey 'E')
  lkd <- keyDown (CharKey 'A')
  rkd <- keyDown (CharKey 'D')
  let spd = calcSpeed <$> lkd <*> rkd

  transfer3 id makeUpdate jkd skd spd

-- | The composed player @'Entity'@ being run by the @'playerController'@.
player :: Signal World -> SignalGen Float (Signal Entity)
player w = playerController w >>= entity initialPlayer
