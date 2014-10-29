-- | This module provides a controller for running an entity to represent a
--   player within the game world.
module WhatIsThisGame.Controllers.Player where

--------------------
-- Global Imports --
import Graphics.UI.GLFW as GLFW
import Control.Applicative
import Control.Monad.Fix
import FRP.Elerea.Param
import Control.Lens
import Linear.V2

-------------------
-- Local Imports --
import WhatIsThisGame.Animation
import WhatIsThisGame.Entity
import WhatIsThisGame.Input
import WhatIsThisGame.Utils
import WhatIsThisGame.Data

----------
-- Code --

-- | The starting position of the player.
playerPosition :: V2 Float
playerPosition = V2 5 30

-- | The size of a player.
playerSize :: V2 Float
playerSize = V2 12 12

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
yAccel :: SignalGen Float (Signal Float)
yAccel = do
  ukd <- keyDown (CharKey 'W')
  dkd <- keyDown (CharKey 'S')

  return $ yAccel' <$> ukd <*> dkd
  where yAccel' :: Bool -> Bool -> Float
        yAccel'  True False =  playerAccelSpeed
        yAccel' False  True = -playerAccelSpeed
        yAccel' False False =  0
        yAccel'  True  True =  0

-- | The velocity in the y-axis.
yVelocity :: Signal Bool -> SignalGen Float (Signal Float)
yVelocity sbounce = do
  sacc  <- yAccel
  sgMap bound $ transfer2 0 yVelocity' sbounce sacc
  where yVelocity' :: Float -> Bool -> Float -> Float -> Float
        yVelocity' dt b acc vel
          | acc == 0  = vel / 1.3
          | otherwise =
            if b
              then -vel / 1.5
              else vel + acc * dt

        bound :: Float -> Float
        bound vel
          | vel >= -minSpeed && vel <= minSpeed =  0
          | vel >   maxSpeed                    =  maxSpeed
          | vel <  -maxSpeed                    = -maxSpeed
          | otherwise                           =  vel

-- | Checking if a position needs to be bounced.
bounce :: Signal Float -> SignalGen p (Signal Bool)
bounce spos = do
  ssize <- renderSize
  return $ bounce' <$> spos <*> fmap (^. _y) ssize
  where bounce' :: Float -> Float -> Bool
        bounce' pos h = pos < 0 || pos + (playerSize ^. _y) > h

-- | The position in the y-axis.
yPosition :: Float -> Signal Float -> SignalGen Float (Signal Float)
yPosition y spos = do
  ssize   <- renderSize
  sbounce <- bounce spos
  svel    <- yVelocity sbounce
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

-- | Constructing the function to transform an @'Entity'@.
makeUpdate :: Float -> Bool -> EntityTransform
makeUpdate pos skd =
  \e -> Just e { getPosition = getPosition e & _y .~ pos
               , shouldShoot = skd
               }

-- | An alternate version of @'initialPlayer'@ that always centers the player.
initialPlayer :: Float -> Entity
initialPlayer y =
  Entity { getName     = "res/player/01.png"
         , getPosition = V2 5 (y / 2)
         , getSize     = playerSize
         , getHealth   = 150
         , shouldShoot = False
         }

-- | Making the player react to whether or not it should shoot.
shootPlayer :: SignalGen Float (Signal EntityTransform)
shootPlayer = do
  sskd <- keyDown (CharKey ' ')
  return $ fmap (\skd -> \e -> Just e { shouldShoot = skd }) sskd

-- | Moving the palyer.
movePlayer :: Float -> SignalGen Float (Signal EntityTransform)
movePlayer y = do
  spos <- mfix $ yPosition y
  return $ fmap (\pos -> \e -> Just e { getPosition = getPosition e & _y .~ pos }) spos

-- | The transform to animate the player.
animatePlayer :: SignalGen Float (Signal EntityTransform)
animatePlayer =
  animateTransform True animation 0.2
  where animation = Animation [ "res/player/01.png"
                              , "res/player/02.png"
                              , "res/player/03.png"
                              , "res/player/04.png"
                              ]

-- | The controller for the player.
playerController :: Float -> SignalGen Float (Signal EntityTransform)
playerController y   = shootPlayer
                   !!= movePlayer y
                   !!= animatePlayer

-- | The composed player @'Entity'@ being run by the @'playerController'@.
player :: Float -> Signal World -> SignalGen Float (Signal (Maybe Entity))
player y _ = do
  playerController y >>= entity (initialPlayer y)
