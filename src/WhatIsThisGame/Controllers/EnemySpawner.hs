-- | This module contains logic to manage the spawning and updating of enemies
--   within the game.
module WhatIsThisGame.Controllers.EnemySpawner (enemies) where

--------------------
-- Global Imports --
import FRP.Elerea.Param

-------------------
-- Local Imports --
import WhatIsThisGame.Data

----------
-- Code --

-- | The list of simulated enemies.
enemies :: Signal World -> SignalGen Float (Signal [Entity])
enemies _ = return $ return []
