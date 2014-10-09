-- | This module handles the creation of new bullets.
module WhatIsThisGame.Controllers.Bullet where

--------------------
-- Global Imports --
import Control.Applicative
import FRP.Elerea.Param
import Control.Lens
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

{--- | Creating a new @'SignalGen'@ for a bullet given input.-}
{-makeBulletGenerator :: Signal BulletType -> Signal (V2 Float) -> Signal Bool -> SignalGen Float (Signal (Maybe (SignalGen Float Bullet)))-}
{-makeBulletGenerator _ _ _ = undefined-}
