-- | This module contains logic to manage the spawning and updating of enemies
--   within the game.
module WhatIsThisGame.Controllers.EnemySpawner (enemies) where

--------------------
-- Global Imports --
import Control.Applicative
import Control.Monad.Fix
import FRP.Elerea.Param
import Control.Monad
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

-- | Generating a new enemy position.
enemyPosition :: SignalGen Float (Signal (V2 Float))
enemyPosition = do
  sH <- sgMap (^. _y) renderSize

  sX <- sgMap (^. _x) renderSize
  sY <- fmap join $ generator $ (\h -> randomRGen (0, h)) <$> sH

  return $ V2 <$> sX <*> sY

-- | Making a new enemy.
newEnemy :: V2 Float -> Float -> Entity
newEnemy pos s =
  Entity { getName     = "res/player/01.png"
         , getPosition = pos - size / 2
         , getSize     = size
         , getHealth   = 1
         , shouldShoot = False
         }
  where size = V2 (s * 1.618) s

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

-- | Maybe adding a new @'Entity'@ to the list.
maybeAppendEnemy :: Bool -> V2 Float -> Float -> [Maybe Entity] -> [Maybe Entity]
maybeAppendEnemy False   _    _ l = l
maybeAppendEnemy  True pos size l =
  (Just $ newEnemy pos size) : l

-- | The list of simulated enemies.
maybeEnemies :: Signal World -> Signal Bool -> Signal [Maybe Entity] -> SignalGen Float (Signal [Maybe Entity])
maybeEnemies _ sMake sMes = do
  sPos  <- enemyPosition
  sSize <- randomRGen (1, 4)
  dt    <- input
  
  delay [] $ stepEnemies <$> dt <*> (maybeAppendEnemy <$> sMake <*> sPos <*> sSize <*> sMes)
  where maybeEnemies' :: Entity -> Float -> Bool -> [Maybe Entity] -> [Maybe Entity]
        maybeEnemies' _ dt False l = stepEnemies dt l
        maybeEnemies' e dt  True l = maybeEnemies' e dt False (Just e : l)

-- | The list of simulated enemies - with all of the dead ones filtered out.
enemies :: Signal World -> SignalGen Float (Signal [Entity])
enemies sw = do
  ss <- shouldSpawn
  sgMap (catMaybes) $ mfix $ maybeEnemies sw ss
