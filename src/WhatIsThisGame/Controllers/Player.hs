-- | This module provides a controller for running an entity to represent a
--   player within the game world.
{-# LANGUAGE Arrows #-}
module WhatIsThisGame.Controllers.Player where

--------------------
-- Global Imports --
import Graphics.UI.GLFW as GLFW
import Prelude hiding ((.), id)
import Control.Lens
import Control.Wire
import Linear.V2

-------------------
-- Local Imports --
import WhatIsThisGame.Utils.Compose
import WhatIsThisGame.Animation
import WhatIsThisGame.Entity
import WhatIsThisGame.Input
import WhatIsThisGame.Data

----------
-- Code --

-- | The initial state that the player should take.
initialPlayer :: Float -> Entity
initialPlayer y =
  Entity { getName     = "res/player/01.png"
         , getPosition = V2 5 y
         , getSize     = playerSize
         , getHealth   = 150
         , shouldShoot = False
         }

-- | The starting position of the player.
playerPosition :: V2 Float
playerPosition = V2 5 30

-- | The size of a player.
playerSize :: V2 Float
playerSize = V2 20 10

-- | The acceleration speed of the player.
playerAccelSpeed :: Float
playerAccelSpeed = 400

-- | The maximum speed.
maxSpeed :: Float
maxSpeed = 100

-- | The minimum speed.
minSpeed :: Float
minSpeed = playerAccelSpeed / 100

-- | The acceleration in the y-axis.
yAccel :: Monoid e => Wire s e IO Float Float
yAccel  =  mkSF_ decel              . keyDown (CharKey 'W') . keyDown (CharKey 'S')
       <|> pure ( playerAccelSpeed) . keyDown (CharKey 'W')
       <|> pure (-playerAccelSpeed) . keyDown (CharKey 'S')
       <|> mkSF_ decel
  where decel :: Float -> Float
        decel v = -v * 2

-- | The velocity in the y-axis.
yVelocity :: HasTime t s => Float -> Wire s e IO (Float, Bool) Float
yVelocity iv =
  mkSF $ \ds (a, b) ->
    let dt = realToFrac $ dtime ds
        v = acc dt iv a b in
      v `seq` (iv, yVelocity v)
  where acc :: Float -> Float -> Float -> Bool -> Float
        acc dt v a b =
          if b
            then bound $ -v / 2
            else bound $ v + a * dt

        bound :: Float -> Float
        bound v
          | v < -maxSpeed                 = -maxSpeed
          | v >  maxSpeed                 =  maxSpeed
          | v > -minSpeed && v < minSpeed =  0
          | otherwise                     =  v


-- | The back-end of the position.
yPosition' :: HasTime t s => Float -> Wire s e IO (Float, Float, Bool) Float
yPosition' iy =
  mkSF $ \ds (v, h, b) ->
    let dt = realToFrac $ dtime ds
        y = move dt iy h v b in
      y `seq` (iy, yPosition' y)
  where move :: Float -> Float -> Float -> Float -> Bool -> Float
        move dt y h v b =
          if b
            then if y < h / 2
              then 0
              else h - playerSize ^. _y
            else y + dt * v

-- | The front-end of the position.
yPosition :: (HasTime t s, Monoid e) => Float -> Wire s e IO Bool Float
yPosition iy =
  proc b -> do
    h <- fmap (^. _y) renderSize -< undefined

    rec a <- delay 0 . yAccel -< v
        v <- yVelocity 0   -< (a, b)
        p <- yPosition' iy -< (v, h, b)

    returnA -< p

-- | Checking if the player should bounce.
bounce :: Wire s e IO Float Bool
bounce =
  mkSF_ bounce' . liftA2 (,) mkId (fmap (^. _y) renderSize)
  where bounce' :: (Float, Float) -> Bool
        bounce' (y, h) =
          y < 0 || y + (playerSize ^. _y) > h

-- | Making the player shoot.
shootPlayer :: Monoid e => Wire s e IO a EntityTransform
shootPlayer  =  pure   shoot . keyDown (CharKey ' ')
            <|> pure unshoot
  where shoot   = \e -> e { shouldShoot = True  }
        unshoot = \e -> e { shouldShoot = False }

-- | The transform to animate the player.
animatePlayer :: HasTime t s => Wire s e IO a EntityTransform
animatePlayer =
  animateTransform True animation 0.1
  where animation = Animation [ "res/player/01.png"
                              , "res/player/02.png"
                              , "res/player/03.png"
                              , "res/player/04.png"
                              ]

-- | The transform for moving the player.
movePlayer :: (HasTime t s, Monoid e) => Float -> Wire s e IO a EntityTransform
movePlayer iy =
  proc _ -> do
    rec p <- delay iy    . yPosition iy -< b
        b <- delay False . bounce       -< p

    returnA -< \e -> e { getPosition = getPosition e & _y .~ p }

-- | The player controller.
playerController :: (HasTime t s, Monoid e) => Wire s e IO World EntityTransform
playerController  = shootPlayer
                 !. animatePlayer
                 !. movePlayer 30

-- | The actual player itself.
player :: (HasTime t s, Monoid e) => Wire s e IO World Entity
player = entity (initialPlayer 30) . playerController
