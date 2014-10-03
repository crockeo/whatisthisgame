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
import WhatIsThisGame.Entity
import WhatIsThisGame.Input
import WhatIsThisGame.Data

----------
-- Code --

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
yAccel :: Signal Float -> SignalGen Float (Signal Float)
yAccel svel = do
  ukd <- keyDown (CharKey 'W')
  dkd <- keyDown (CharKey 'S')

  return $ yAccel' <$> ukd <*> dkd <*> svel
  where yAccel' :: Bool -> Bool -> Float -> Float
        yAccel'  True False   _ =  playerAccelSpeed
        yAccel' False  True   _ = -playerAccelSpeed
        yAccel'   ukd   dkd vel
          | vel < 0   = if ukd then  playerAccelSpeed * 2 else  playerAccelSpeed
          | vel > 0   = if dkd then -playerAccelSpeed * 2 else -playerAccelSpeed
          | otherwise =  0

-- | The velocity in the y-axis.
yVelocity :: Signal Bool -> Signal Float -> SignalGen Float (Signal Float)
yVelocity sbounce svel = do
  sacc  <- yAccel svel
  svel' <- transfer2 0 yVelocity' sbounce sacc
  delay 0 svel'
  where yVelocity' :: Float -> Bool -> Float -> Float -> Float
        yVelocity' dt b acc vel =
          bound $ if b
            then -vel / 1.5
            else  vel + acc * dt

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

-- | An alternate version of @'initialPlayer'@ that always centers the player.
initialPlayer :: Float -> Entity
initialPlayer y =
  Entity { getName     = "res/player.png"
         , getPosition = V2 5 (y / 2)
         , getSize     = playerSize
         , getHealth   = 150
         , shouldShoot = False
         }

-- | An alternate version of the @'playerController'@.
playerController :: Float -> Signal World -> SignalGen Float (Signal EntityTransform)
playerController y _ = do
  spos <- mfix $ yPosition y
  sskd <- keyDown (CharKey ' ')

  return $ makeUpdate <$> spos <*> sskd

-- | The composed player @'Entity'@ being run by the @'playerController'@.
player :: Signal World -> SignalGen Float (Signal Entity)
player w = do
  y <- renderSize >>= (fmap calcPos . snapshot)
  playerController y w >>= entity (initialPlayer y)
  where calcPos :: V2 Float -> Float
        calcPos (V2 _ h) = (h / 2) - (playerSize ^. _y / 2)
