-- | This module provides some defaults for entity control in addition to some
--   more convenient type synonyms.
module WhatIsThisGame.EntityUpdate where

--------------------
-- Global Imports --
import Graphics.UI.GLFW as GLFW
import Control.Applicative
import FRP.Elerea.Param

-------------------
-- Local Imports --
import WhatIsThisGame.Input
import WhatIsThisGame.Data

----------
-- Code --

-- | The controller for the player.
playerController :: Signal Float -> SignalGen p (Signal EntityUpdate)
playerController sd = do
  jkd <- keyDown (CharKey ' ')
  skd <- keyDown (CharKey 'E')
  lkd <- keyDown (CharKey 'A')
  rkd <- keyDown (CharKey 'D')

  pure $ EntityUpdate <$> jkd <*> skd <*> sd <*> (calcSpeed <$> lkd <*> rkd)
  where calcSpeed :: Bool -> Bool -> Float
        calcSpeed False False = 3
        calcSpeed  True  True = 3
        calcSpeed False  True = playerMoveSpeed + playerMoveSpeed / 2
        calcSpeed  True False = playerMoveSpeed - playerMoveSpeed / 2