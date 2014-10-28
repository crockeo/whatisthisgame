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
import WhatIsThisGame.Collision
import WhatIsThisGame.Input
import WhatIsThisGame.Utils
import WhatIsThisGame.Data

----------
-- Code --

-- | The rate for a new enemy to spawn. (In other words, the time it takes for a
--   new enemy to spawn.)
spawnRate :: Float
spawnRate = 1

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
  Entity { getName     = "res/enemy.png"
         , getPosition = pos - size / 2
         , getSize     = size
         , getHealth   = 1
         , shouldShoot = False
         }
  where size = V2 (s * 1.618) s

-- | Stepping a single @'Entity'@ that represents an enemy.
stepEnemy :: Float -> (Maybe Entity, [Bullet]) -> Maybe Entity
stepEnemy dt (me, bus) =
  case me of
    Nothing -> Nothing
    Just e  ->
      let width  = getSize e ^. _x
          newPos = getPosition e + V2 (-20 * dt) 0 in
        if (newPos ^. _x) + width < 0
          then Nothing
          else findDeath bus $ e { getPosition = newPos }
  where findDeath :: [Bullet] -> Entity -> Maybe Entity
        findDeath []     e = Just e
        findDeath (b:bs) e =
          if looseCollides b e
            then Nothing
            else findDeath bs e

-- | Stepping a whole list of enemies.
stepEnemies :: Float -> [Bullet] -> [Maybe Entity] -> [Maybe Entity]
stepEnemies dt bus =
  map (stepEnemy dt) . pair bus
  where pair :: b -> [a] -> [(a, b)]
        pair _    []  = []
        pair b (a:as) = (a, b) : pair b as

-- | Maybe adding a new @'Entity'@ to the list.
maybeAppendEnemy :: Bool -> V2 Float -> Float -> [Maybe Entity] -> [Maybe Entity]
maybeAppendEnemy False   _    _ l = l
maybeAppendEnemy  True pos size l =
  (Just $ newEnemy pos size) : l

-- | The list of simulated enemies.
maybeEnemies :: Signal World -> Signal Bool -> Signal [Maybe Entity] -> SignalGen Float (Signal [Maybe Entity])
maybeEnemies sWorld sMake sMes = do
  sPos  <- enemyPosition
  sSize <- randomRGen (5, 10)
  dt    <- input
  
  delay [] $ stepEnemies <$> dt <*> fmap worldGetBullets sWorld <*>
    (maybeAppendEnemy <$> sMake <*> sPos <*> sSize <*> sMes)

-- | The list of simulated enemies - with all of the dead ones filtered out.
enemies :: Signal World -> SignalGen Float (Signal [Entity])
enemies sw = do
  ss <- shouldSpawn
  sgMap (catMaybes) $ mfix $ maybeEnemies sw ss
