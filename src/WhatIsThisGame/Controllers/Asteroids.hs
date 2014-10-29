-- | This module handles the creation and animation on background asteroids.
module WhatIsThisGame.Controllers.Asteroids (asteroids) where

--------------------
-- Global Imports --
import FRP.Elerea.Param
import Data.Maybe
import Linear.V2

-------------------
-- Local Imports --
import WhatIsThisGame.Animation
import WhatIsThisGame.Entity
import WhatIsThisGame.Utils
import WhatIsThisGame.Data

----------
-- Code --

-- | Creating a new asteroid.
newAsteroid :: V2 Float -> V2 Float -> Entity
newAsteroid pos size =
  Entity { getName     = "res/asteroid/01/01.png"
         , getPosition = pos
         , getSize     = size
         , getHealth   = 0
         , shouldShoot = False
         }

-- | A list of initial, tiled asteroids.
initialAsteroids :: [Entity]
initialAsteroids =
  [ newAsteroid (V2 10 10) (V2 10 10)
  ]

-- | Animating an asteroid.
animateAsteroid :: Animation -> SignalGen Float (Signal EntityTransform)
animateAsteroid anim = animateTransform True anim 0.1

-- | The controller for updating the background.
asteroidsController :: Signal World -> SignalGen Float ([Signal EntityTransform])
asteroidsController _ =
  fmap repeat $ animateAsteroid anim
  where anim = Animation [ "res/asteroid/01/01.png"
                         , "res/asteroid/01/02.png"
                         , "res/asteroid/01/03.png"
                         , "res/asteroid/01/04.png"
                         , "res/asteroid/01/05.png"
                         , "res/asteroid/01/06.png"
                         , "res/asteroid/01/07.png"
                         , "res/asteroid/01/08.png"
                         , "res/asteroid/01/09.png"
                         , "res/asteroid/01/10.png"
                         , "res/asteroid/01/11.png"
                         , "res/asteroid/01/12.png"
                         , "res/asteroid/01/13.png"
                         , "res/asteroid/01/14.png"
                         , "res/asteroid/01/15.png"
                         ]

-- | The list of @'Maybe'@ asteroids. Just used to satisfy the type
--   constraint.
maybeAsteroids :: Signal World -> SignalGen Float (Signal [Maybe Entity])
maybeAsteroids w = asteroidsController w >>= (entities . zip initialAsteroids)

-- | The list of asteroids with the @'Maybe'@s removed.
asteroids :: Signal World -> SignalGen Float (Signal [Entity])
asteroids = sgMap catMaybes . maybeAsteroids
