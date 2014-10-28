-- | This module contains logic to manage the spawning and updating of enemies
--   within the game.
module WhatIsThisGame.Controllers.EnemySpawner (enemies) where

--------------------
-- Global Imports --
import Control.Applicative
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

-- | The rate for a new enemy to spawn. (In other words, the time it takes for a
--   new enemy to spawn.)
spawnRate :: Float
spawnRate = 0.75

-- | Returns @'True'@ when a new enemy *should* spawn.
shouldSpawn :: SignalGen Float (Signal Bool)
shouldSpawn = periodically spawnRate $ pure True

-- | Asking for a new enemy.
newEnemy :: Float -> SignalGen p Entity
newEnemy x = do
  rY <- randomRGen (40, 50)
  rS <- randomRGen (1, 1)

  return $ Entity { getName     = "res/player/01.png"
                  , getPosition = V2 x rY
                  , getSize     = V2 (rS * 1.618) rS
                  , getHealth   = 1
                  , shouldShoot = False
                  }

-- | Stepping a single @'Entity'@ that represents an enemy.
stepEnemy :: Float -> Maybe Entity -> Maybe Entity
stepEnemy dt me =
  case me of
    Nothing -> Nothing
    Just e  ->
      let width  = getSize e ^. _x
          newPos = getPosition e + V2 (-20 * dt) 0 in
        if (newPos ^. _x) + width < 0
          then Nothing
          else Just $ e { getPosition = newPos }

-- | Stepping a whole list of enemies.
stepEnemies :: Float -> [Maybe Entity] -> [Maybe Entity]
stepEnemies dt = map (stepEnemy dt)

-- | The list of simulated enemies.
maybeEnemies :: Signal World -> Signal Bool -> SignalGen Float (Signal [Maybe Entity])
maybeEnemies _ sMake = do
  ne <- newEnemy 90
  transfer [] (maybeEnemies' ne) sMake
  where maybeEnemies' :: Entity -> Float -> Bool -> [Maybe Entity] -> [Maybe Entity]
        maybeEnemies' _ dt False l = stepEnemies dt l
        maybeEnemies' e dt  True l = maybeEnemies' e dt False (Just e : l)

-- | The list of simulated enemies - with all of the dead ones filtered out.
enemies :: Signal World -> SignalGen Float (Signal [Entity])
enemies sw = sgMap catMaybes $ shouldSpawn >>= maybeEnemies sw
