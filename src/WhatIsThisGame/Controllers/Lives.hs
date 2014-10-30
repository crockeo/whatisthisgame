-- | This module calculates and updates the amount of lives that the player
--   currently has.
module WhatIsThisGame.Controllers.Lives ( initialLives
                                        , calculateLives
                                        ) where

--------------------
-- Global Imports --
import FRP.Elerea.Param
import Control.Lens
import Linear.V2

import Debug.Trace

-------------------
-- Local Imports --
import WhatIsThisGame.Data

----------
-- Code --

-- | The initial number of lives.
initialLives :: Int
initialLives = 10

-- | Checking if any enemies will die this tick.
anyWillDie :: [Entity] -> Bool
anyWillDie = foldl (\d e -> d || (getPosition e + getSize e) ^. _x - 1 < 0) False

-- | The amount of lives the player has.
calculateLives :: Signal Bool -> SignalGen Float (Signal Int)
calculateLives =
  transfer initialLives calculateLives'
  where calculateLives' :: Float -> Bool -> Int -> Int
        calculateLives' _ False n = traceShow n n
        calculateLives' _  True n = n - 1