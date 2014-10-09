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

-- | Getting the delta time in a @'SignalGen' ('Signal' 'Float')@.
deltaTime :: SignalGen Float (Signal Float)
deltaTime =
  stateful 0 deltaTime'
  where deltaTime' :: Float -> Float -> Float
        deltaTime' dt _ = dt
