-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Where the board is drawn, and what a click lands on.
module GeometryTest
  ( tests
  )
where

import           Hedgehog
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range

import           Go.Types
import           Stones.Goban.Geometry

-- | A board, a widget size, and a point on that board.
scene :: Gen (Int, Double, Double, Coord)
scene = do
  n      <- Gen.element [9, 13, 19]
  width  <- Gen.double (Range.linearFrac 200 1600)
  height <- Gen.double (Range.linearFrac 200 1600)
  x      <- Gen.int (Range.linear 0 (n - 1))
  y      <- Gen.int (Range.linear 0 (n - 1))
  pure (n, width, height, Coord x y)

prop_aClickOnAPointLandsOnThatPoint :: Property
prop_aClickOnAPointLandsOnThatPoint = property $ do
  (n, width, height, coord) <- forAll scene
  let geo    = geometry n width height
      (x, y) = centreOf geo coord
  pointAt geo x y === Just coord

prop_aClickNearAPointStillLandsOnIt :: Property
prop_aClickNearAPointStillLandsOnIt = property $ do
  (n, width, height, coord) <- forAll scene
  let geo    = geometry n width height
      (x, y) = centreOf geo coord
      -- Just inside half a step, which is the half of the board that
      -- belongs to this point.
      nudge  = geo.step * 0.4
  pointAt geo (x + nudge) (y - nudge) === Just coord

prop_theBoardIsASquareInTheMiddle :: Property
prop_theBoardIsASquareInTheMiddle = property $ do
  (n, width, height, _) <- forAll scene
  let geo = geometry n width height
  assert (geo.side <= min width height + 0.001)
  assert (geo.left >= -0.001)
  assert (geo.top >= -0.001)
  diff (geo.left * 2 + geo.side) (\a b -> abs (a - b) < 0.001) width
  diff (geo.top * 2 + geo.side) (\a b -> abs (a - b) < 0.001) height

prop_aClickOffTheBoardLandsNowhere :: Property
prop_aClickOffTheBoardLandsNowhere = withTests 1 . property $ do
  let geo = geometry 19 600 600
  pointAt geo (-50) 300 === Nothing
  pointAt geo 300 (-50) === Nothing
  pointAt geo 650 300 === Nothing
  pointAt geo 300 650 === Nothing

prop_theStarPointsAreTheOnesOnABoard :: Property
prop_theStarPointsAreTheOnesOnABoard = withTests 1 . property $ do
  length (starPoints 19) === 9
  length (starPoints 13) === 5
  length (starPoints 9) === 5
  length (starPoints 5) === 0
  -- The 4-4 point in the top left, and the middle of the board.
  assert (Coord 3 3 `elem` starPoints 19)
  assert (Coord 9 9 `elem` starPoints 19)
  assert (Coord 15 15 `elem` starPoints 19)
  -- A 9x9 board marks the 3-3 points and the middle.
  assert (Coord 2 2 `elem` starPoints 9)
  assert (Coord 4 4 `elem` starPoints 9)

tests :: Group
tests = $$(discover)
