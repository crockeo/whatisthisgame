-- | This module provides some tools to run through an @'Animation'@ in the
--   scope of FRP and Haskell.
{-# LANGUAGE Arrows #-}
module WhatIsThisGame.Animation ( accumTime
                                , accumTimeLoop
                                , accumTimeNoLoop
                                , stepArray
                                , animate
                                , animateTransform
                                ) where

--------------------
-- Global Imports --
import Control.Monad.Fix (MonadFix)
import Prelude hiding ((.))
import Control.Wire

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
accumTime :: (HasTime t s, Monad m) => Float -> Wire s e m Bool Float
accumTime it =
  mkSF $ \ds r ->
    if r
      then (it, accumTime 0)
      else
        let dt = realToFrac $ dtime ds
            t' = it + dt in
          t' `seq` (it, accumTime t')

-- | Looping between 0 and a given point in time.
accumTimeLoop :: (HasTime t s, MonadFix m) => Float -> Float -> Wire s e m a Float
accumTimeLoop it mt =
  proc _ -> do
    rec t <- accumTime it . mkSF_ (above mt) -< t
    returnA -< t
  where above :: Float -> Float -> Bool
        above cap n = n >= cap

-- | Counting up infinitely from 0 (in real time).
accumTimeNoLoop :: (HasTime t s, Monad m) => Float -> Wire s e m a Float
accumTimeNoLoop it =
  accumTime it . pure False

-- | The back-end of @'stepArray'@.
stepArray' :: (HasTime t s, Monad m) => Wire s e m a Float -> [b] -> Float -> Wire s e m a b
stepArray' timer l s =
  proc _ -> do
    t <- timer                              -< undefined
    i <- mkSF_ ((!!) l . bound l . index s) -< t
    returnA -< i

-- | Stepping through an array in real-time.
stepArray :: (HasTime t s, MonadFix m) => Bool -> [b] -> Float -> Wire s e m a b
stepArray  True l s = stepArray' (accumTimeLoop   0 $ s * fromIntegral (length l)) l s
stepArray False l s = stepArray' (accumTimeNoLoop 0)                               l s

-- | Stepping through an animation at a given arte.
animate :: (HasTime t s, MonadFix m) => Bool -> Animation -> Float -> Wire s e m a String
animate sloop (Animation list) step = stepArray sloop list step

-- | Creating an @'EntityTransform'@ for an @'Animation'@.
animateTransform :: (HasTime t s, MonadFix m) => Bool -> Animation -> Float -> Wire s e m a EntityTransform
animateTransform sloop animation step =
  mkSF_ makeTransform . animate sloop animation step
  where makeTransform :: String -> EntityTransform
        makeTransform name = \e -> e { getName = name }

{-

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
  return $ fmap (\frame -> \e -> e { getName = frame }) sframe

-}
