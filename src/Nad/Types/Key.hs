-- | Key combinations, and the names used for them in configuration and on the
-- command line.
module Nad.Types.Key
  ( Modifier (..)
  , KeyCode (..)
  , KeyCombo (..)
  , modifierBit
  , packModifiers
  , unpackModifiers
  , keyByName
  , nameOfKey
  , parseCombo
  , showCombo
  , conflicting
  ) where

import Data.Bits ((.&.), (.|.))
import Data.List (find, intercalate, sort)
import Data.Word (Word16, Word32)

data Modifier = Cmd | Alt | Ctrl | Shift
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | A macOS virtual key code. These describe a physical key position, not the
-- character it produces, so they do not change with the keyboard layout.
newtype KeyCode = KeyCode Word16
  deriving (Eq, Ord, Show)

data KeyCombo = KeyCombo
  { comboMods :: ![Modifier]
  , comboKey :: !KeyCode
  }
  deriving (Eq, Show)

-- | Must match the NAD_MOD_* constants in @cbits/nad.h@.
modifierBit :: Modifier -> Word32
modifierBit m = case m of
  Cmd -> 1
  Alt -> 2
  Ctrl -> 4
  Shift -> 8

packModifiers :: [Modifier] -> Word32
packModifiers = foldr ((.|.) . modifierBit) 0

unpackModifiers :: Word32 -> [Modifier]
unpackModifiers bits = [m | m <- [minBound .. maxBound], bits .&. modifierBit m /= 0]

-- | US-layout virtual key codes. Only the keys worth binding are listed; add
-- more here and they become available to both the config and the CLI.
keyTable :: [(String, KeyCode)]
keyTable =
  map (fmap KeyCode) $
    letters
      <> digits
      <> [ ("return", 36)
         , ("tab", 48)
         , ("space", 49)
         , ("delete", 51)
         , ("escape", 53)
         , ("left", 123)
         , ("right", 124)
         , ("down", 125)
         , ("up", 126)
         , ("comma", 43)
         , ("period", 47)
         , ("slash", 44)
         , ("minus", 27)
         , ("equal", 24)
         , ("grave", 50)
         ]
  where
    letters =
      [ ("a", 0), ("b", 11), ("c", 8), ("d", 2), ("e", 14), ("f", 3), ("g", 5)
      , ("h", 4), ("i", 34), ("j", 38), ("k", 40), ("l", 37), ("m", 46)
      , ("n", 45), ("o", 31), ("p", 35), ("q", 12), ("r", 15), ("s", 1)
      , ("t", 17), ("u", 32), ("v", 9), ("w", 13), ("x", 7), ("y", 16), ("z", 6)
      ]
    digits =
      [ ("1", 18), ("2", 19), ("3", 20), ("4", 21), ("5", 23)
      , ("6", 22), ("7", 26), ("8", 28), ("9", 25), ("0", 29)
      ]

keyByName :: String -> Maybe KeyCode
keyByName name = lookup name keyTable

nameOfKey :: KeyCode -> Maybe String
nameOfKey code = fst <$> find ((== code) . snd) keyTable

modifierNames :: [(String, Modifier)]
modifierNames = [("cmd", Cmd), ("alt", Alt), ("ctrl", Ctrl), ("shift", Shift)]

-- | @\"cmd-alt-j\"@. Modifiers first in any order, then exactly one key.
parseCombo :: String -> Maybe KeyCombo
parseCombo input = case reverse (splitOn '-' input) of
  [] -> Nothing
  (keyPart : modParts) -> do
    key <- keyByName keyPart
    mods <- mapM (`lookup` modifierNames) (reverse modParts)
    -- Sorted so that "alt-cmd-j" and "cmd-alt-j" are the same binding.
    pure (KeyCombo (sort mods) key)

showCombo :: KeyCombo -> String
showCombo (KeyCombo mods key) =
  intercalate "-" (map name (sort mods) <> [maybe "?" id (nameOfKey key)])
  where
    name m = maybe "?" fst (find ((== m) . snd) modifierNames)

-- | Which of the system's shortcuts stand in the way of the given bindings.
--
-- macOS dispatches its own shortcuts before any event tap, so a binding that
-- matches one can never fire. The ids come back so the caller can switch those
-- shortcuts off — and, just as importantly, switch them back on afterwards.
conflicting :: [KeyCombo] -> [(Int, KeyCombo)] -> [Int]
conflicting wanted system = [hotkeyId | (hotkeyId, combo) <- system, combo `elem` wanted]

splitOn :: Char -> String -> [String]
splitOn sep s = case break (== sep) s of
  (chunk, []) -> [chunk]
  (chunk, _ : rest) -> chunk : splitOn sep rest
