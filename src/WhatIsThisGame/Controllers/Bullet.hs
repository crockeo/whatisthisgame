-- | This module handles the creation of new bullets.
module WhatIsThisGame.Controllers.Bullet where

--------------------
-- Global Imports --
import Control.Applicative
import Control.Monad.Fix
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

-- | Maybe making a new bullet. Depends on the @'Bool'@.
maybeMakeBullet :: Bool -> BulletType -> V2 Float -> Maybe Bullet
maybeMakeBullet False  _   _ = Nothing
maybeMakeBullet  True bt pos = Just $ makeBullet bt pos

-- | Simulating a bullet.
stepBullet :: Bullet -> SignalGen Float (Signal (Maybe Bullet))
stepBullet ib = do
  ssize <- renderSize
  transfer (Just ib) step $ fmap (^. _x) ssize
  where step :: Float -> Float -> Maybe Bullet -> Maybe Bullet
        step  _ _ Nothing  = Nothing
        step dt _ (Just b) = Just $ b { getBulletPosition = getBulletPosition b + getBulletSpeed b * pure dt }

-- | Simulating a list of bullets.
stepBullets :: Signal Bool -> Signal BulletType -> Signal (V2 Float) -> SignalGen Float (Signal [Maybe Bullet])
stepBullets sMake sBType sPos =
  mfix stepBullets'
  where stepBullets' :: Signal [Maybe Bullet] -> SignalGen Float (Signal [Maybe Bullet])
        stepBullets' sMBullets = do
          let newBullet = maybeMakeBullet <$> sMake <*> sBType <*> sPos
          delay [] $ insertMaybe <$> sMBullets <*> newBullet

        insertMaybe :: [Maybe Bullet] -> Maybe Bullet -> [Maybe Bullet]
        insertMaybe l Nothing = l
        insertMaybe l       b = b : l

-- | The real bullets.
bullets :: Signal Bool -> Signal BulletType -> Signal (V2 Float) -> SignalGen Float (Signal [Bullet])
bullets sMake sBType sPos = do
  mBus <- stepBullets sMake sBType sPos
  return $ fmap catMaybes mBus
