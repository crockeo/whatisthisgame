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

-- | Composing functions contained with @'SignalGen'@s using @'(>>=)'@.
(!!=) :: Monad m
      => SignalGen p (Signal (b -> m c))
      -> SignalGen p (Signal (a -> m b))
      -> SignalGen p (Signal (a -> m c))
(!!=) sgb sga = do
  sa <- sga
  sb <- sgb
  
  return $ do
    fa <- sa
    fb <- sb
    
    return (\a -> do
      b <- fa a
      fb b)
  

-- | Mapping a function over a @'SignalGen'@.
sgMap :: (a -> b) -> SignalGen Float (Signal a) -> SignalGen Float (Signal b)
sgMap fn sg =
  fmap (fmap fn) sg

-- | Getting the total time that's passed since the network started.
totalTime :: SignalGen Float (Signal Float)
totalTime =
  stateful 0 totalTime'
  where totalTime' :: Float -> Float -> Float
        totalTime' dt t = t + dt
        
-- | Getting the player's current score.
currentScore :: SignalGen Float (Signal Int)
currentScore = sgMap floor totalTime

-- | A @'SignalGen'@ that ticks every so often with a @'True'@.
periodically :: Float -> Signal Bool -> SignalGen Float (Signal Bool)
periodically cap sb =
  sgMap snd $ transfer (0, False) periodically' sb
  where periodically' :: Float -> Bool -> (Float, Bool) -> (Float, Bool)
        periodically' dt b (t, _) =
          let t' = t + dt in
            if b && t' >= cap
              then (0 ,  True)
              else (t', False)
