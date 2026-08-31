-- | An ordered window list with a focus — nad's equivalent of xmonad's
-- @StackSet@, minus the parts that only make sense under X11.
--
-- The order is the layout order: the head is the master window. macOS owns the
-- real focus, so 'stackFocus' is nad's intent, reconciled with reality by
-- 'sync' whenever the window list is re-read.
module Nad.Core.Stack
  ( Stack (..)
  , empty
  , fromList
  , sync
  , focusNext
  , focusPrev
  , swapNext
  , swapPrev
  , swapMaster
  , insert
  , remove
  , setFocus
  ) where

import Data.List (elemIndex)
import Data.Maybe (listToMaybe)

data Stack a = Stack
  { stackItems :: ![a]
  , stackFocus :: !(Maybe a)
  }
  deriving (Eq, Show)

empty :: Stack a
empty = Stack [] Nothing

fromList :: [a] -> Stack a
fromList xs = Stack xs (listToMaybe xs)

-- | Reconcile with the windows that actually exist.
--
-- Known windows keep their position, so re-reading the window list never
-- reshuffles the layout. New windows go to the front — they are what the user
-- just opened and wants to work in. A focus on a window that has gone falls
-- back to the master.
sync :: Eq a => [a] -> Stack a -> Stack a
sync live (Stack order focused) = Stack items focus
  where
    kept = filter (`elem` live) order
    fresh = filter (`notElem` kept) live
    items = fresh <> kept
    focus = case focused of
      Just f | f `elem` items -> Just f
      _ -> listToMaybe items

insert :: Eq a => a -> Stack a -> Stack a
insert x s
  | x `elem` stackItems s = s
  | otherwise = Stack (x : stackItems s) (Just x)

remove :: Eq a => a -> Stack a -> Stack a
remove x (Stack items focused) = Stack rest focus
  where
    rest = filter (/= x) items
    focus = case focused of
      Just f | f /= x -> Just f
      _ -> listToMaybe rest

setFocus :: Eq a => a -> Stack a -> Stack a
setFocus x s
  | x `elem` stackItems s = s {stackFocus = Just x}
  | otherwise = s

focusNext :: Eq a => Stack a -> Stack a
focusNext = shiftFocus 1

focusPrev :: Eq a => Stack a -> Stack a
focusPrev = shiftFocus (-1)

-- | Focus wraps around: past the end of the stack is the master again.
shiftFocus :: Eq a => Int -> Stack a -> Stack a
shiftFocus step s@(Stack items _) = case (items, focusIndex s) of
  ([], _) -> s
  (_, Nothing) -> s {stackFocus = listToMaybe items}
  (_, Just i) -> s {stackFocus = Just (items !! ((i + step) `mod` length items))}

swapNext :: Eq a => Stack a -> Stack a
swapNext = swapBy 1

swapPrev :: Eq a => Stack a -> Stack a
swapPrev = swapBy (-1)

-- | Move the focused window through the order, wrapping. The focus travels
-- with the window rather than staying at the position.
swapBy :: Eq a => Int -> Stack a -> Stack a
swapBy step s@(Stack items _) = case focusIndex s of
  Nothing -> s
  Just i -> s {stackItems = swapAt i ((i + step) `mod` length items) items}

-- | Promote the focused window to master. When it already is master, put the
-- second window there instead, so the same key toggles between two windows.
swapMaster :: Eq a => Stack a -> Stack a
swapMaster s@(Stack items _) = case focusIndex s of
  Nothing -> s
  Just 0 | length items > 1 -> s {stackItems = swapAt 0 1 items, stackFocus = Just (items !! 1)}
  Just 0 -> s
  Just i -> s {stackItems = swapAt 0 i items}

focusIndex :: Eq a => Stack a -> Maybe Int
focusIndex (Stack items focused) = focused >>= (`elemIndex` items)

-- | Both indices come from positions in @xs@, so they are always in range.
swapAt :: Int -> Int -> [a] -> [a]
swapAt i j xs
  | i == j = xs
  | otherwise = [pick k x | (k, x) <- zip [0 ..] xs]
  where
    pick k x
      | k == i = xs !! j
      | k == j = xs !! i
      | otherwise = x
