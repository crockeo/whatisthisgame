-- | A set of utilities for working with @'Signal'@s and @'SignalGen'@s.
module WhatIsThisGame.Utils where

--------------------
-- Global Imports --
import Control.Applicative
import FRP.Elerea.Param

----------
-- Code --

-- | Running @'(.)'@ on a function contained within a @'Signal'@.
(!.) :: Signal (b -> c) -> Signal (a -> b) -> Signal (a -> c)
(!.) s1 s2 = (.) <$> s1 <*> s2

-- | Running @'(.)'@ on a function contained within a @'SignalGen'@.
(!!.) :: SignalGen p (Signal (b -> c)) -> SignalGen p (Signal (a -> b)) -> SignalGen p (Signal (a -> c))
(!!.) s1 s2 = (!.) <$> s1 <*> s2

-- | Mapping a function over a @'SignalGen'@.
sgMap :: (a -> b) -> SignalGen Float (Signal a) -> SignalGen Float (Signal b)
sgMap fn sg =
  fmap (fmap fn) sg

-- | Getting the delta time in a @'SignalGen' ('Signal' 'Float')@.
deltaTime :: SignalGen Float (Signal Float)
deltaTime =
  stateful 0 deltaTime'
  where deltaTime' :: Float -> Float -> Float
        deltaTime' dt _ = dt

-- | A @'SignalGen'@ that ticks every so often with a @'True'@.
periodically :: Float -> SignalGen Float (Signal Bool)
periodically cap =
  sgMap snd $ stateful (0, False) periodically'
  where periodically' :: Float -> (Float, Bool) -> (Float, Bool)
        periodically' dt (t, _) =
          let t' = t + dt in
            if t' >= cap
              then (0 ,  True)
              else (t', False)
