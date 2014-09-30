-- | This module provides some defaults for entity control in addition to some
--   more convenient type synonyms.
module WhatIsThisGame.EntityTransform (playerController) where

--------------------
-- Global Imports --
import Graphics.UI.GLFW as GLFW
import Control.Applicative
import FRP.Elerea.Param
import Control.Lens
import Linear.V2

-------------------
-- Local Imports --
import WhatIsThisGame.Input
import WhatIsThisGame.Data

----------
-- Code --

calcSpeed :: Bool -> Bool -> Float
calcSpeed False False = playerMoveSpeed
calcSpeed  True  True = playerMoveSpeed
calcSpeed False  True = playerMoveSpeed + playerMoveSpeed / 2
calcSpeed  True False = playerMoveSpeed - playerMoveSpeed / 2

makeUpdate :: Float -> Bool -> Bool -> Float -> a -> EntityTransform
makeUpdate dt _ _ sp _ =
  \e -> e { getPosition = getPosition e & _x .~ getPosition e ^. _x + sp * dt }

-- | An alternate version of the @'playerController'@.
playerController :: SignalGen Float (Signal EntityTransform)
playerController = do
  jkd <- keyDown (CharKey ' ')
  skd <- keyDown (CharKey 'E')
  lkd <- keyDown (CharKey 'A')
  rkd <- keyDown (CharKey 'D')
  let spd = calcSpeed <$> lkd <*> rkd

  transfer3 id makeUpdate jkd skd spd
