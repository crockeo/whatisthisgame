-- | This module contains logic to manage the spawning and updating of enemies
--   within the game.
module WhatIsThisGame.Controllers.EnemySpawner (enemies) where

--------------------
-- Global Imports --
import FRP.Elerea.Param
import Control.Lens
import Data.Maybe
import Linear.V2

-------------------
-- Local Imports --
import WhatIsThisGame.Input
import WhatIsThisGame.Utils
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
step :: Float -> [Maybe Entity] -> [Maybe Entity]
step dt es =
  map step' es
  where step' :: Maybe Entity -> Maybe Entity
        step' me =
          case me of
            Nothing  -> Nothing
            Just e   -> 
              let width  = getSize e ^. _x
                  newPos = getPosition e + V2 (-20 * dt) 0 in
                if (newPos ^. _x) + width < 0
                  then Nothing
                  else Just $ e { getPosition = newPos }

-- | Stepping a bunch of entities.
stepEntities :: [Maybe Entity] -> SignalGen Float (Signal [Maybe Entity])
stepEntities es = stateful es step

-- | The list of simulated enemies.
maybeEnemies :: Signal World -> SignalGen Float (Signal [Maybe Entity])
maybeEnemies _ = sequence [fmap Just $ newEnemy x | x <- replicate 20 150] >>= stepEntities

-- | The list of simulated enemies - with all of the dead ones filtered out.
enemies :: Signal World -> SignalGen Float (Signal [Entity])
enemies = sgMap catMaybes . maybeEnemies
