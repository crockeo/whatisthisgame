-- | This module provides the API reason about collision. Namely it implements
--   a function to check if two @'CollisionRectangle'@s collide, and extends
--   that to work with any type that implements the @'Collidable'@ class.
module WhatIsThisGame.Collision where

--------------------
-- Global Imports --
import Linear.V2

-------------------
-- Local Imports --
import WhatIsThisGame.Data

----------
-- Code --

-- | Getting a side of a @'CollisionRectangle'@.
top, bottom, left, right :: CollisionRectangle -> Float
top    (CollisionRectangle (V2 _ y) (V2 _ h)) = y + h
bottom (CollisionRectangle (V2 _ y) (V2 _ _)) = y
left   (CollisionRectangle (V2 x _) (V2 _ _)) = x
right  (CollisionRectangle (V2 x _) (V2 w _)) = x + w

-- | Checking if two @'CollisionRectangle'@s collide.
collides :: CollisionRectangle -> CollisionRectangle -> Bool
collides cr1 cr2 =
  right cr1 > left   cr2 && left   cr1 < right cr2 &&
  top   cr1 > bottom cr2 && bottom cr1 < top   cr2

-- | Checking if two @'Collidable'@s collide. They don't need to be the same
--   type.
looseCollides :: (Collidable a, Collidable b) => a -> b -> Bool
looseCollides a b =
  collides (toCollisionRectangle a)
           (toCollisionRectangle b)