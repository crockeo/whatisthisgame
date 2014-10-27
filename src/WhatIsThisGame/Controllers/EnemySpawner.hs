-- | This module contains logic to manage the spawning and updating of enemies
--   within the game.
module WhatIsThisGame.Controllers.EnemySpawner (enemies) where

--------------------
-- Global Imports --
import FRP.Elerea.Param
import Control.Monad
import Linear.V2

-------------------
-- Local Imports --
import WhatIsThisGame.Input
import WhatIsThisGame.Data

----------
-- Code --

-- | Asking for a new enemy.
newEnemy :: Float -> SignalGen p Entity
newEnemy x = do
  rY <- randomRGen (0, 480)
  rW <- randomRGen (5, 50)
  rH <- randomRGen (5, 50)

  return $ Entity { getName     = "res/player/01.png"
                  , getPosition = V2 x rY
                  , getSize     = V2 rW rH
                  , getHealth   = 1
                  , shouldShoot = False
                  }

-- | The list of simulated enemies.
enemies :: Signal World -> SignalGen Float (Signal [Entity])
enemies _ =
  fmap return $ sequence [newEnemy x | x <- [1..50]]
