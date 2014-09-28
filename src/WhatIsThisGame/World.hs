-- | This module provides the logic for constructing and rendering the game
--   world.
module WhatIsThisGame.World where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL
import FRP.Elerea.Param
import Linear.V2

-------------------
-- Local Imports --
import WhatIsThisGame.Data

----------
-- Code --

-- | Providing the rendering for a @'World'@.
instance Renderable World where
  render World =
    renderPrimitive Quads $
      mapM_ linearVertex $ generateVertices (V2 (-0.5) (-0.5)) (V2 1 1)
    where linearVertex :: V2 Float -> IO ()
          linearVertex (V2 x y) =
            vertex $ Vertex2 (realToFrac x :: GLfloat)
                             (realToFrac y :: GLfloat)

          generateVertices :: V2 Float -> V2 Float -> [V2 Float]
          generateVertices (V2 x y) (V2 w h) =
            [ V2 (x    ) (y    )
            , V2 (x + w) (y    )
            , V2 (x + w) (y + h)
            , V2 (x    ) (y + h)
            ]

-- | Providing an always-updated @'World'@.
world :: SignalGen Float (Signal World)
world = return $ return World
