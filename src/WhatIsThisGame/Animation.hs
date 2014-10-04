-- | This module provides some tools to run through an @'Animation'@ in the
--   scope of FRP and Haskell.
module WhatIsThisGame.Animation ( stepArray
                                , animate
                                ) where

--------------------
-- Global Imports --
import Control.Applicative
import Control.Monad.Fix
import FRP.Elerea.Param

-------------------
-- Local Imports --
import WhatIsThisGame.Data

----------
-- Code --

-- | Making sure the index is bound by the size of the list.
bound :: [a] -> Int -> Int
bound l n =
  if n >= length l
    then length l - 1
    else n

-- | Finding the index of the array given the time step and the accumulated
--   time.
index :: Float -> Float -> Int
index little big = floor $ big / little

-- | Accumulating time.
accumTime :: Signal Bool -> SignalGen Float (Signal Float)
accumTime sreset =
  transfer 0 accumTime' sreset
  where accumTime' :: Float -> Bool -> Float -> Float
        accumTime'  _ True _ = 0
        accumTime' dt    _ t = dt + t

-- | Creating a timer that loops between 0 and a given value.
accumTimeLoop :: Float -> SignalGen Float (Signal Float)
accumTimeLoop cap =
  mfix accumTimeLoop'
  where accumTimeLoop' :: Signal Float -> SignalGen Float (Signal Float)
        accumTimeLoop' t = do
          at <- accumTime $ (>) <$> t <*> pure cap
          delay 0 $ at

-- | Creating a timer that NEVER resets.
accumTimeNoLoop :: SignalGen Float (Signal Float)
accumTimeNoLoop = accumTime $ pure False

-- | The back end to stepping the array, given a @'SignalGen' 'Float' ('Signal' 'Float')@
stepArray' :: SignalGen Float (Signal Float) -> [a] -> Float -> SignalGen Float (Signal a)
stepArray' tgen list step =
  liftA (\t -> (!!) <$> pure list <*> liftA (bound list . index step) t) tgen

-- | Stepping through a list at a given rate (with the option of looping).
stepArray :: Bool -> [a] -> Float -> SignalGen Float (Signal a)
stepArray False list step = stepArray' (accumTimeNoLoop                                  ) list step
stepArray  True list step = stepArray' (accumTimeLoop (fromIntegral (length list) * step)) list step

-- | Stepping through an animation at a given rate.
animate :: Bool -> Animation -> Float -> SignalGen Float (Signal String)
animate loop (Animation list) step = stepArray loop list step

-- | Creating an entity transform to set the sprite to the current sprite in
--   the animation.
animateTransform :: Bool -> Animation -> Float -> SignalGen Float (Signal EntityTransform)
animateTransform loop anim step = do
  sframe <- animate loop anim step
  return $ fmap (\frame -> \e -> { getName = frame }) sframe
