-- | This module provides a custom API for rendering text within the game. It
--   works by loading every single glyph (pretty much just ASCII characters)
--   I'll be using as a Sprite and then rendering them in the correct positions
--   when necessary.
module WhatIsThisGame.Text where

--------------------
-- Global Imports --
import Data.Monoid

-------------------
-- Local Imports --
import WhatIsThisGame.Data

----------
-- Code --

-- | The prefix to where the glyphs will be contained.
glyphPrefix :: String
glyphPrefix = "res/glyphs/"

-- | Converting a character into its corresponding image name.
toImageName :: Char -> String
toImageName '(' = "oparen"
toImageName ')' = "cparen"
toImageName '.' = "dot"
toImageName ',' = "comma"
toImageName  c  = [c]

-- | Generating a single asset load.
generateAssetLocation :: String -> Char -> String
generateAssetLocation prefix c = prefix ++ toImageName c ++ ".png"

-- | Getting the proper sprite from the @'Assets'@ for the corresponding
--   character.
getCharacterSprite :: Assets -> Char -> Sprite
getCharacterSprite assets c =
  getSprites assets ! generateAssetLocation glyphPrefix c

-- | The asset loads for the text.
glyphLoads :: AssetLoad
glyphLoads =
  mconcat $
    map (SpriteLoad . generateAssetLocation "res/glyphs/") ('(' : ')' : ',' : '.' : ['a' .. 'z'])