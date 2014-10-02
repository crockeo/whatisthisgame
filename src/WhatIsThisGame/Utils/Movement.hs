-- | A module to help with the movement of entities within Elerea.
module WhatIsThisGame.Utils.Movement where

--------------------
-- Global Imports --
import Control.Applicative
import Control.Monad.Fix
import FRP.Elerea.Param
import Control.Lens
import Linear.V2

----------
-- Code --

-- | Integrating a value with respect to time.
integrate :: Float -> Signal Float -> SignalGen Float (Signal Float)
integrate iv sspd =
  mfix $ integrate'
  where integrate' :: Signal Float -> SignalGen Float (Signal Float)
        integrate' sv =
          delay iv $ (+) <$> sv <*> sspd

-- | Calculating acceleration to a velocity. Generic for both axes.
velocity :: Signal Float -> SignalGen Float (Signal Float)
velocity saccel = integrate 0 saccel

-- | Calculating acceleration to a velocity in a 2D space.
velocity2D :: Signal (V2 Float) -> SignalGen Float (Signal (V2 Float))
velocity2D saccels = do
  vx <- velocity $ fmap (^. _x) saccels
  vy <- velocity $ fmap (^. _y) saccels

  return $ V2 <$> vx <*> vy

-- | Given the initial position and the acceleration, calculate the position in
--   1D space.
position :: Float -> Signal Float -> SignalGen Float (Signal Float)
position pos saccel = do
  v <- velocity saccel
  integrate pos v

-- | Given the initial position and the acceleration, calculate the position in
--   2D space.
position2D :: V2 Float -> Signal (V2 Float) -> SignalGen Float (Signal (V2 Float))
position2D poss saccels = do
  vx <- velocity $ fmap (^. _x) saccels
  vy <- velocity $ fmap (^. _y) saccels

  px <- integrate (poss ^. _x) vx
  py <- integrate (poss ^. _y) vy

  return $ V2 <$> px <*> py
