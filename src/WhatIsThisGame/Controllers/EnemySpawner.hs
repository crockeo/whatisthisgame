-- | This module contains logic to manage the spawning and updating of enemies
--   within the game.
module WhatIsThisGame.Controllers.EnemySpawner (enemies) where

--------------------
-- Global Imports --
import FRP.Elerea.Param
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
  rS <- randomRGen (1, 1)

  return $ Entity { getName     = "res/player/01.png"
                  , getPosition = V2 x rY
                  , getSize     = V2 (rS * 1.618) rS
                  , getHealth   = 1
                  , shouldShoot = False
                  }

-- | Stepping an entity
step :: Float -> [Entity] -> [Entity]
step dt es =
  map step' es
  where step' :: Entity -> Entity
        step' e =
          e { getPosition = getPosition e + V2 (-20 * dt) 0 }

-- | Stepping a bunch of entities.
stepEntities :: [Entity] -> SignalGen Float (Signal [Entity])
stepEntities es = stateful es step

-- | The list of simulated enemies.
enemies :: Signal World -> SignalGen Float (Signal [Entity])
enemies _ = sequence [newEnemy x | x <- replicate 20 150] >>= stepEntities
