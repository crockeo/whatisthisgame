-- | This module calculates and updates the amount of lives that the player
--   currently has.
module WhatIsThisGame.Controllers.Lives ( initialLives
                                        , calculateLives
                                        ) where

--------------------
-- Global Imports --
import FRP.Elerea.Param

----------
-- Code --

-- | The initial number of lives.
initialLives :: Int
initialLives = 10

-- | The amount of lives the player has.
calculateLives :: Signal Bool -> SignalGen Float (Signal Int)
calculateLives =
  transfer initialLives calculateLives'
  where calculateLives' :: Float -> Bool -> Int -> Int
        calculateLives' _ False n = n
        calculateLives' _  True n = n - 1
