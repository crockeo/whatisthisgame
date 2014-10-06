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

-- | Checking if the player should bounce.
bounce :: Wire s e IO Float Bool
bounce =
  mkSF_ bounce' . liftA2 (,) mkId (fmap (^. _y) renderSize)
  where bounce' :: (Float, Float) -> Bool
        bounce' (y, h) =
          y < 0 || y + (playerSize ^. _y) > h

-- | The acceleration in the y-axis.
yAccel :: Monoid e => Wire s e IO Float Float
yAccel  =  mkSF_ decel              . keyDown (CharKey 'W') . keyDown (CharKey 'S')
       <|> pure ( playerAccelSpeed) . keyDown (CharKey 'W')
       <|> pure (-playerAccelSpeed) . keyDown (CharKey 'S')
       <|> mkSF_ decel
  where decel :: Float -> Float
        decel v
          | v < 0     =  playerAccelSpeed
          | v > 0     = -playerAccelSpeed
          | otherwise = 0

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
            then -v / 1.5
            else bound $ v + a * dt

        bound :: Float -> Float
        bound v
          | v < -maxSpeed                 = -maxSpeed
          | v >  maxSpeed                 =  maxSpeed
          | v > -minSpeed && v < minSpeed =  0
          | otherwise                     =  v


-- | The position in the y-axis.
yPosition :: HasTime t s => Float -> Wire s e IO (Float, Float, Bool) Float
yPosition iy =
  mkSF $ \ds (v, h, b) ->
    let dt = realToFrac $ dtime ds
        y = move dt iy h v b in
      y `seq` (iy, yPosition y)
  where move :: Float -> Float -> Float -> Float -> Bool -> Float
        move dt y h v b =
          if b
            then closer y 0 h
            else y + dt * v

        closer :: (Ord a, Num a) => a -> a -> a -> a
        closer a t1 t2 =
          snd $ max d1 d2
          where d1 = (abs (t1 - a), t2)
                d2 = (abs (t2 - a), t2)

-- | Making the player shoot.
shootPlayer :: Monoid e => Wire s e IO a EntityTransform
shootPlayer  =  pure   shoot . keyDown (CharKey ' ')
            <|> pure unshoot
  where shoot   = \e -> e { shouldShoot = True  }
        unshoot = \e -> e { shouldShoot = False }

-- | Animating the player.
animatePlayer :: Wire s e IO a EntityTransform
animatePlayer = pure id

-- | The transform for moving the player.
movePlayer :: (HasTime t s, Monoid e) => Float -> Wire s e IO a EntityTransform
movePlayer iy =
  movePlayer' . fmap (^. _y) renderSize
  where movePlayer' :: (HasTime t s, Monoid e) => Wire s e IO Float EntityTransform
        movePlayer' =
          proc h -> do
            rec a <- yAccel       -< v
                v <- yVelocity 0  -< (a, b)
                p <- yPosition iy -< (v, h, b)
                b <- bounce       -< p

            et <- movePlayer'' -< p

            returnA -< et

        movePlayer'' :: Wire s e IO Float EntityTransform
        movePlayer'' =
          mkSF_ $ \y ->
            \e -> e { getPosition = getPosition e & _y .~ y }

-- | The actual player itself.
playerController :: (HasTime t s, Monoid e) => Wire s e IO World EntityTransform
playerController =
  proc w -> do
    sp <- shootPlayer   -< w
    ap <- animatePlayer -< w
    mp <- movePlayer 30 -< w

    returnA -< foldl1 (.) [ sp
                          , ap
                          , mp
                          ]

player :: (HasTime t s, Monoid e) => Wire s e IO World Entity
player = entity (initialPlayer 30) . playerController

{-

-- | The position in the y-axis.
yPosition :: Float -> Signal Float -> SignalGen Float (Signal Float)
yPosition y spos = do
  ssize   <- renderSize
  sbounce <- bounce spos
  svel    <- mfix $ yVelocity sbounce
  spos'   <- transfer2 y yPosition' svel $ fmap (^. _y) ssize
  delay y $ spos'
  where yPosition' :: Float -> Float -> Float -> Float -> Float
        yPosition' dt vel size pos =
          let pos' = pos + vel * dt in
            bound pos' size

        bound :: Float -> Float -> Float
        bound pos size
          | pos                      <    0 = 0
          | pos + (playerSize ^. _y) > size = size - (playerSize ^. _y)
          | otherwise                       = pos

-- | Calculating the speed of the player.
calcSpeed :: Bool -> Bool -> Float
calcSpeed False False = playerMoveSpeed
calcSpeed  True  True = playerMoveSpeed
calcSpeed False  True = playerMoveSpeed + playerMoveSpeed / 2
calcSpeed  True False = playerMoveSpeed - playerMoveSpeed / 2

-- | Constructing the function to transform an @'Entity'@.
makeUpdate :: Float -> Bool -> EntityTransform
makeUpdate pos skd =
  \e -> e { getPosition = getPosition e & _y .~ pos
          , shouldShoot = skd
          }

-- | Making the player react to whether or not it should shoot.
shootPlayer :: SignalGen Float (Signal EntityTransform)
shootPlayer = do
  sskd <- keyDown (CharKey ' ')
  return $ fmap (\skd -> \e -> e { shouldShoot = skd }) sskd

-- | Moving the palyer.
movePlayer :: Float -> SignalGen Float (Signal EntityTransform)
movePlayer y = do
  spos <- mfix $ yPosition y
  return $ fmap (\pos -> \e -> e { getPosition = getPosition e & _y .~ pos }) spos

-- | The transform to animate the player.
animatePlayer :: SignalGen Float (Signal EntityTransform)
animatePlayer =
  animateTransform True animation 0.1
  where animation = Animation [ "res/player/01.png"
                              , "res/player/02.png"
                              , "res/player/03.png"
                              , "res/player/04.png"
                              ]

-- | The controller for the player.
playerController :: Float -> SignalGen Float (Signal EntityTransform)
playerController y   = shootPlayer
                   !!. movePlayer y
                   !!. animatePlayer

-- | The composed player @'Entity'@ being run by the @'playerController'@.
player :: Signal World -> SignalGen Float (Signal Entity)
player _ = do
  y <- renderSize >>= (fmap calcPos . snapshot)
  playerController y >>= entity (initialPlayer y)
  where calcPos :: V2 Float -> Float
        calcPos (V2 _ h) = (h / 2) - (playerSize ^. _y / 2)

-}
