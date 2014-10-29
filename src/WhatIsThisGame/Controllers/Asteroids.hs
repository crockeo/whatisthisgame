-- | This module handles the creation and animation on background asteroids.
module WhatIsThisGame.Controllers.Asteroids (asteroids) where

--------------------
-- Global Imports --
import FRP.Elerea.Param
import System.IO.Unsafe
import System.Random
import Data.Maybe
import Linear.V2

-------------------
-- Local Imports --
import WhatIsThisGame.Animation
import WhatIsThisGame.Entity
import WhatIsThisGame.Utils
import WhatIsThisGame.Data

------------
-- Unsafe --
instance Random a => Random (V2 a) where
  random g =
    let (x, g' ) = random g
        (y, g'') = random g' in
      (V2 x y, g'')
  
  randomR (V2 xmi ymi, V2 xma yma) g =
    let (x, g' ) = randomR (xmi, xma) g
        (y, g'') = randomR (ymi, yma) g' in
      (V2 x y, g'')
  
-- | A list of initial, tiled asteroids.
initialAsteroids :: [Entity]
initialAsteroids =
  map (\p -> newAsteroid p $ V2 5 5) positions
  where positions = unsafePerformIO $ sequence $ replicate 5 $ randomRIO (V2 0 0, V2 100 100)
          
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
