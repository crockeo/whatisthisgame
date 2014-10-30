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

-- | The base speed for the enemies.
baseSpeed :: Float
baseSpeed = -15

-- | The maximum speed for enemies.
maxSpeed :: Float
maxSpeed = -115

-- | The current speed for the enemies.
currentSpeed :: SignalGen Float (Signal Float)
currentSpeed = do
  t <- totalTime
  return $ currentSpeed' baseSpeed <$> t
  where currentSpeed' :: Float -> Float -> Float
        currentSpeed' bs t =
          let t' = bs * (t ** 0.3) in
            if t' < maxSpeed
              then maxSpeed
              else t'

-- | Returns @'True'@ when a new enemy *should* spawn.
shouldSpawn :: SignalGen Float (Signal Bool)
shouldSpawn = periodically spawnRate $ pure True

-- | Generating a new enemy position.
enemyPosition :: SignalGen Float (Signal (V2 Float))
enemyPosition = do
  sH <- sgMap (^. _y) renderSize
  let sOffset = fmap (/10) sH

  sX <- sgMap (^. _x) renderSize
  sY <- fmap join $ generator $ (\o h -> randomRGen (o, h - o)) <$> sOffset <*> sH

  return $ V2 <$> sX <*> sY

-- | Making a new enemy.
newEnemy :: V2 Float -> Float -> Entity
newEnemy pos s =
  Entity { getName     = "res/enemy/01.png"
         , getPosition = pos - size / 2
         , getSize     = size
         , getHealth   = 1
         , shouldShoot = False
         }
  where size = V2 (s * 1.618) s

-- | Stepping a single @'Entity'@ that represents an enemy.
stepEnemy :: Float -> Float -> (Maybe Entity, [Bullet]) -> (Maybe Entity, Bool)
stepEnemy dt spd (me, bus) =
  case me of
    Nothing -> (Nothing, False)
    Just e  ->
      let width  = getSize e ^. _x
          newPos = getPosition e + V2 (spd * dt) 0 in
        if (newPos ^. _x) + width < 0
          then (Nothing, True)
          else (findDeath bus $ e { getPosition = newPos }, False)
  where findDeath :: [Bullet] -> Entity -> Maybe Entity
        findDeath []     e = Just e
        findDeath (b:bs) e =
          if looseCollides b e
            then Nothing
            else findDeath bs e

-- | Stepping a whole list of enemies.
stepEnemies :: Float -> Float -> [Bullet] -> [Maybe Entity] -> ([Maybe Entity], Bool)
stepEnemies dt spd bus =
  orSnd . unzip . map (stepEnemy dt spd) . pair bus
  where pair :: b -> [a] -> [(a, b)]
        pair _    []  = []
        pair b (a:as) = (a, b) : pair b as
        
        orSnd :: (a, [Bool]) -> (a, Bool)
        orSnd (a, bs) = (a, foldl (||) False bs)

-- | Maybe adding a new @'Entity'@ to the list.
maybeAppendEnemy :: Bool -> V2 Float -> Float -> [Maybe Entity] -> [Maybe Entity]
maybeAppendEnemy False   _    _ l = l
maybeAppendEnemy  True pos size l =
  (Just $ newEnemy pos size) : l

-- | The list of simulated enemies.
maybeEnemies :: Signal World
             -> Signal Bool
             -> Signal ([Maybe Entity], Bool)
             -> SignalGen Float (Signal ([Maybe Entity], Bool))
maybeEnemies sWorld sMake sBlob = do
  let sMes = fmap fst sBlob
  sPos  <- enemyPosition
  sSpd  <- currentSpeed
  sSize <- randomRGen (5, 10)
  dt    <- input
  
  let mes = stepEnemies <$> dt <*> sSpd <*> fmap worldGetBullets sWorld <*>
              (maybeAppendEnemy <$> sMake <*> sPos <*> sSize <*> sMes)
  
  delay ([], False) mes

-- | Mapping a value over the first signal in a pair of signals contained within
--   a SignalGen
mapFst :: (a -> c) -> SignalGen Float (Signal (a, b)) -> SignalGen Float (Signal (c, b))
mapFst fn = sgMap (\(a, b) -> (fn a, b))

-- | The list of simulated enemies - with all of the dead ones filtered out.
enemies :: Signal World -> SignalGen Float (Signal ([Entity], Bool))
enemies sw = do
  ss <- shouldSpawn
  mapFst catMaybes $ mfix $ maybeEnemies sw ss
