module Compose where

--------------------
-- Global Imports --
import Control.Applicative
import FRP.Elerea.Param

----------
-- Code --

-- | Running @'(.)'@ on a function contained within a @'Signal'@.
(!.) :: Signal (b -> c) -> Signal (a -> b) -> Signal (a -> b)
(!.) s1 s2 = (.) <$> s1 <*> s2

-- | Running @'(.)'@ on a function contained within a @'SignalGen'@.
(!!.) :: SignalGen p (Signal (b -> c)) -> SignalGen p (Signal (a -> b)) -> SignalGen p (Signal (a -> c))
(!!.) s1 s2 = (!.) <$> s1 <*> s2
