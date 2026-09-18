-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | The rules that act on one stone at a time.
module BoardTest
  ( tests
  )
where

import qualified Data.Set                      as Set
import           Hedgehog

import           Go.Board
import           Go.Types

-- | Stop the test and say why, which is what a move the rules refuse
-- calls for: there is nothing to go on with.
giveUp :: MonadTest m => String -> m a
giveUp why = annotate why >> failure

-- | Put these stones down without alternating, which is how a shape is
-- set up for a test.
setUp :: MonadTest m => [(Color, (Int, Int))] -> Board -> m Board
setUp [] board = pure board
setUp ((color, (x, y)) : rest) board =
  case place color (Coord x y) board of
    Left reason -> giveUp ("refused " <> show (color, x, y, reason))
    Right placement -> setUp rest placement.after

prop_emptyBoardIsEmpty :: Property
prop_emptyBoardIsEmpty = withTests 1 . property $ do
  let board = emptyBoard 9
  length (coords board) === 81
  stoneCount Black board === 0
  stoneCount White board === 0

prop_cornerStoneHasTwoLiberties :: Property
prop_cornerStoneHasTwoLiberties = withTests 1 . property $ do
  board <- setUp [(Black, (0, 0))] (emptyBoard 9)
  Set.size (liberties board (group board (Coord 0 0))) === 2

prop_capturesASurroundedStone :: Property
prop_capturesASurroundedStone = withTests 1 . property $ do
  -- White on D4, Black on three of its sides. The fourth closes it.
  board <- setUp
    [ (White, (3, 3))
    , (Black, (2, 3))
    , (Black, (4, 3))
    , (Black, (3, 2))
    ]
    (emptyBoard 9)
  case place Black (Coord 3 4) board of
    Left  reason    -> giveUp ("refused: " <> show reason)
    Right placement -> do
      placement.captured === Set.singleton (Coord 3 3)
      stoneAt placement.after (Coord 3 3) === Nothing

prop_capturesAWholeGroup :: Property
prop_capturesAWholeGroup = withTests 1 . property $ do
  -- Two white stones in the corner, with all but one liberty taken.
  board <- setUp
    [ (White, (0, 0))
    , (White, (1, 0))
    , (Black, (0, 1))
    , (Black, (1, 1))
    ]
    (emptyBoard 9)
  case place Black (Coord 2 0) board of
    Left  reason    -> giveUp ("refused: " <> show reason)
    Right placement -> do
      placement.captured
        === Set.fromList [Coord 0 0, Coord 1 0]
      stoneCount White placement.after === 0

prop_refusesSuicide :: Property
prop_refusesSuicide = withTests 1 . property $ do
  -- The corner point is surrounded by Black, so White cannot fill it.
  board <- setUp [(Black, (0, 1)), (Black, (1, 0))] (emptyBoard 9)
  place White (Coord 0 0) board === Left Suicide

prop_allowsFillingWhenItCaptures :: Property
prop_allowsFillingWhenItCaptures = withTests 1 . property $ do
  -- Two white stones sit either side of the corner, each with the
  -- corner as its only liberty:
  --
  -- >    0 1 2
  -- >  0 . W B
  -- >  1 W B .
  -- >  2 B . .
  --
  -- Black plays the corner. On its own that stone would have no
  -- liberties, but both white stones come off first, which gives it
  -- two, so the move is legal.
  board <- setUp
    [ (White, (1, 0))
    , (White, (0, 1))
    , (Black, (2, 0))
    , (Black, (1, 1))
    , (Black, (0, 2))
    ]
    (emptyBoard 9)
  case place Black (Coord 0 0) board of
    Left  reason    -> giveUp ("refused: " <> show reason)
    Right placement -> do
      placement.captured === Set.fromList [Coord 1 0, Coord 0 1]
      stoneAt placement.after (Coord 0 0) === Just Black
      stoneCount White placement.after === 0

prop_refusesOccupiedAndOffBoard :: Property
prop_refusesOccupiedAndOffBoard = withTests 1 . property $ do
  board <- setUp [(Black, (4, 4))] (emptyBoard 9)
  place White (Coord 4 4) board === Left Occupied
  place White (Coord 9 0) board === Left OffBoard
  place White (Coord (-1) 0) board === Left OffBoard

prop_groupFollowsConnectedStones :: Property
prop_groupFollowsConnectedStones = withTests 1 . property $ do
  board <- setUp
    [ (Black, (2, 2))
    , (Black, (3, 2))
    , (Black, (4, 2))
    , (Black, (4, 3))
    , (White, (5, 2))
    ]
    (emptyBoard 9)
  Set.size (group board (Coord 2 2)) === 4
  Set.size (group board (Coord 5 2)) === 1
  Set.null (group board (Coord 8 8)) === True

tests :: Group
tests = $$(discover)
