-- | A set of utilities surrounding the config surrounding the window. This is
--   to be used in creating and resizing the window.
module WhatIsThisGame.WindowConfig ( load
                                   , loadGuaranteed
                                   , makeSize
                                   , makeWindowMode
                                   ) where

--------------------
-- Global Imports --
import Control.Lens hiding (noneOf)
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW as GLFW
import Text.Parsec.String
import System.Directory
import Text.Parsec

-------------------
-- Local Imports --
import WhatIsThisGame.Data

----------
-- Code --

-- | Creating a @'WindowConfig'@ transform out of a 2-tuple of @'String'@s.
makeTransform :: (String, String) -> (WindowConfig -> WindowConfig)
makeTransform (     "width",      width) = \wc -> wc &      cfgWidth .~         read        width
makeTransform (    "height",     height) = \wc -> wc &     cfgHeight .~         read       height
makeTransform ("fullscreen", fullscreen) = \wc -> wc & cfgFullscreen .~         read   fullscreen
makeTransform (   "jumpkey",    jumpKey) = \wc -> wc &    cfgJumpKey .~ toEnum (read      jumpKey)
makeTransform (  "shootkey",   shootKey) = \wc -> wc &   cfgShootKey .~ toEnum (read     shootKey)
makeTransform (   "slowkey",    slowKey) = \wc -> wc &    cfgSlowKey .~ toEnum (read      slowKey)
makeTransform (   "fastkey",    fastKey) = \wc -> wc &    cfgFastKey .~ toEnum (read      fastKey)
makeTransform _ = id

-- | Recovering from an error in loading a file.
recover :: FilePath -> IO WindowConfig
recover path = do
  writeFile path $ show defaultWindowConfig
  return defaultWindowConfig

-- | Parsing out the data for a single line.
flagPair :: Parser (String, String)
flagPair = try $ do
  f <- many1 $ noneOf "=\n\r"
  char '='
  v <- many1 $ noneOf "=\n\r"
  return (f, v)

-- | An alternative to @'flagPair'@ that always returns an empty pair.
emptyPair :: Parser (String, String)
emptyPair = return ("", "")

-- | Parsing out the data for every line.
flagLines :: Parser [(String, String)]
flagLines = sepBy (flagPair <|> emptyPair) (oneOf "\n\r")

-- | Applying all of the functions in a list onto a value.
applyAll :: [a -> a] -> a -> a
applyAll     [] a = a
applyAll (x:xs) a = applyAll xs $ x a

-- | The parser for the @'WindowConfig'@.
windowConfigParser :: Parser WindowConfig
windowConfigParser = do
  fls <- flagLines
  return $ applyAll (map makeTransform fls) defaultWindowConfig

-- | Attempting to parse out a @'WindowConfig'@ out of a string.
parseWindowConfig :: String -> Either String WindowConfig
parseWindowConfig input =
  case parse windowConfigParser "parseWindowConfig" input of
    Left  err -> Left $ show err
    Right val -> Right val

-- | Loading a config file from the system.
load :: FilePath -> IO (Either String WindowConfig)
load path = do
  fe <- doesFileExist path
  if not fe
    then return $ Left "Config file does not exist."
    else fmap parseWindowConfig $ readFile path

-- | Loading a config file from the system, with a guarantee that it *will*
--   always return a @'WindowConfig'@. This is achieved by writing a default
--   config to the filesystem if a failure is seen.
loadGuaranteed :: FilePath -> IO WindowConfig
loadGuaranteed path = do
  ewc <- load path
  either (const $ recover path) (return . id) ewc

-- | Creating a @'Size'@ out of a @'WindowConfig'@.
makeSize :: WindowConfig -> Size
makeSize wc =
  Size (fromIntegral $ wc ^.  cfgWidth)
       (fromIntegral $ wc ^. cfgHeight)

-- | Checking a @'WindowMode'@ from a @'WindowConfig'@.
makeWindowMode :: WindowConfig -> WindowMode
makeWindowMode wc =
  case wc ^. cfgFullscreen of
    False -> Window
    True  -> FullScreen
