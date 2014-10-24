-- | This module handles the creation of new bullets.
module WhatIsThisGame.Controllers.Bullet (bullets) where

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

-- | The size of a bullet.
bulletSize :: V2 Float
bulletSize = V2 3 1.5

-- | The default player bullet.
playerBullet :: V2 Float -> V2 Float -> Bullet
playerBullet pos size =
  Bullet { getBulletType     = PlayerBullet
         , getBulletPosition = pos + (size / 2) - (bulletSize / 2)
         , getBulletSize     = bulletSize
         , getBulletDamage   = 15
         , getBulletSpeed    = V2 5 0
         }

-- | The default enemy bullet.
enemyBullet :: V2 Float -> V2 Float -> Bullet
enemyBullet pos size =
  Bullet { getBulletType     = EnemyBullet
         , getBulletPosition = pos + (size / 2) - (bulletSize / 2)
         , getBulletSize     = bulletSize
         , getBulletDamage   = 15
         , getBulletSpeed    = V2 5 0
         }

-- | Making a bullet from @'BulletType'@.
makeBullet :: BulletType -> V2 Float -> V2 Float -> Bullet
makeBullet PlayerBullet pos size = playerBullet pos size
makeBullet EnemyBullet  pos size = enemyBullet  pos size

-- | Stepping a single bullet.
stepBullet :: Float -> Float -> Bullet -> Maybe Bullet
stepBullet dt w b =
  let np = getBulletPosition b + getBulletSpeed b * pure dt in
    if np ^. _x > w
      then Nothing
      else Just $ b { getBulletPosition = np }

-- | Stepping a whole list of @'Maybe' 'Bullet'@.
stepMaybeBullets :: Float -> Float -> [Maybe Bullet] -> [Maybe Bullet]
stepMaybeBullets  _ _     [] = []
stepMaybeBullets dt w (b:bs) =
  (b >>= stepBullet dt w) : stepMaybeBullets dt w bs

-- | Possibly appending a bulle to a @['Maybe' 'Bullet']@.
maybeAppendBullet :: Bool -> BulletType -> V2 Float -> V2 Float -> [Maybe Bullet] -> [Maybe Bullet]
maybeAppendBullet False          _   _   _ bullets = bullets
maybeAppendBullet  True bulletType pos vel bullets =
  Just (makeBullet bulletType pos vel) : bullets

-- | The same as bullets, but without the @'Maybe'@s filtered out.
maybeBullets :: Signal Bool
             -> Signal BulletType
             -> Signal (V2 Float)
             -> Signal (V2 Float)
             -> Signal [Maybe Bullet]
             -> SignalGen Float (Signal [Maybe Bullet])
maybeBullets sMake sBulletType sPos sVel sBullets = do
  sDt    <- input
  sWidth <- fmap (fmap (^. _x)) renderSize

  delay [] $ stepMaybeBullets <$> sDt <*> sWidth <*>
    (maybeAppendBullet <$> sMake <*> sBulletType <*> sPos <*> sVel <*> sBullets)

-- | Produces bullets given a number of signals describing its creation.
bullets :: Signal Bool -> Signal BulletType -> Signal (V2 Float) -> Signal (V2 Float) -> SignalGen Float (Signal [Bullet])
bullets sMake sBulletType sPos sVel =
  fmap (fmap catMaybes) $ mfix $ maybeBullets sMake sBulletType sPos sVel
