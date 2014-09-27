-- | The module that contains all of the type definitions in the library.
--   They're contained in one place so that recursive module imports may be
--   avoided when only because of type definitions.
module WhatIsThisGame.Types where

----------
-- Code --

-- | Specifying that a type can be rendered.
class Renderable a where
  render :: a -> IO ()
