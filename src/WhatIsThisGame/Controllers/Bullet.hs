-- | This module handles the creation of new bullets.
module WhatIsThisGame.Controllers.Bullet where

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
import WhatIsThisGame.Data

----------
-- Code --

-- | The default player bullet.
playerBullet :: V2 Float -> Bullet
playerBullet pos =
  Bullet { getBulletType     = PlayerBullet
         , getBulletPosition = pos
         , getBulletSize     = V2 3 1.5
         , getBulletDamage   = 15
         , getBulletSpeed    = V2 5 0
         }

-- | The default enemy bullet.
enemyBullet :: V2 Float -> Bullet
enemyBullet pos =
  Bullet { getBulletType     = EnemyBullet
         , getBulletPosition = pos
         , getBulletSize     = V2 3 1.5
         , getBulletDamage   = 15
         , getBulletSpeed    = V2 5 0
         }

-- | Making a bullet from @'BulletType'@.
makeBullet :: BulletType -> V2 Float -> Bullet
makeBullet PlayerBullet pos = playerBullet pos
makeBullet EnemyBullet  pos = enemyBullet  pos

-- | Simulating a bullet.
stepBullet :: Bullet -> SignalGen Float (Signal (Maybe Bullet))
stepBullet ib = do
  ssize <- renderSize
  transfer (Just ib) step $ fmap (^. _x) ssize
  where step :: Float -> Float -> Maybe Bullet -> Maybe Bullet
        step  _ _ Nothing  = Nothing
        step dt _ (Just b) = Just $ b { getBulletPosition = getBulletPosition b + getBulletSpeed b * pure dt }

-- | Turning a list of @'SignalGen'@s into a @'SignalGen'@ of a list.
produceList :: [SignalGen p (Signal a)] -> SignalGen p (Signal [a])
produceList = fmap (sequence) . sequence

-- | Making a list of bullets from signal generators that make a Maybe Bullet.
onlyRealBullets :: [SignalGen Float (Signal (Maybe Bullet))] -> SignalGen Float (Signal [Bullet])
onlyRealBullets = fmap (fmap catMaybes) . produceList

-- | Making a new bullet generator.
makeBulletGenerator :: BulletType
                    -> V2 Float
                    -> SignalGen Float (Signal (Maybe Bullet))
makeBulletGenerator bType pos =
  stepBullet $ makeBullet bType pos

-- | Creating a list of bullet generators.
makeBulletGenerators :: Signal BulletType
                     -> Signal (V2 Float)
                     -> Signal Bool
                     -> SignalGen Float (Signal [SignalGen Float (Signal (Maybe Bullet))])
makeBulletGenerators sBType sPos sMake =
  transfer3 [] makeBulletGenerators' sBType sPos sMake
  where makeBulletGenerators' :: Float
                              -> BulletType
                              -> (V2 Float)
                              -> Bool
                              -> [SignalGen Float (Signal (Maybe Bullet))]
                              -> [SignalGen Float (Signal (Maybe Bullet))]
        makeBulletGenerators' _     _   _ False l = l
        makeBulletGenerators' _ bType pos  True l = makeBulletGenerator bType pos : l

-- | The real bullets.
bullets :: Signal BulletType
        -> Signal (V2 Float)
        -> Signal Bool
        -> SignalGen Float (Signal [Bullet])
bullets sBType sPos sMake = do
  sGens <- makeBulletGenerators sBType sPos sMake
  gens <- snapshot sGens

  onlyRealBullets gens
