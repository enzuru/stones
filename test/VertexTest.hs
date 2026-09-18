-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | The names the protocol gives to points.
module VertexTest
  ( tests
  )
where

import           Hedgehog
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range

import           Go.Types
import           Go.Vertex

-- | A board width and a point on a board that wide.
pointOn :: Gen (Int, Coord)
pointOn = do
  n <- Gen.element [9, 13, 19]
  x <- Gen.int (Range.linear 0 (n - 1))
  y <- Gen.int (Range.linear 0 (n - 1))
  pure (n, Coord x y)

prop_aNameReadsBackAsThePointItNames :: Property
prop_aNameReadsBackAsThePointItNames = property $ do
  (n, coord) <- forAll pointOn
  (fromVertex n =<< toVertex n coord) === Just coord

prop_theColumnsSkipTheLetterI :: Property
prop_theColumnsSkipTheLetterI = withTests 1 . property $ do
  columnLetters 19 === "ABCDEFGHJKLMNOPQRST"
  columnLetters 9 === "ABCDEFGHJ"

prop_theCornersAreWhereTheProtocolSaysTheyAre :: Property
prop_theCornersAreWhereTheProtocolSaysTheyAre = withTests 1 . property $ do
  -- A1 is the bottom left, which is the last row of a board drawn from
  -- the top, and T19 is the top right of a 19x19 board.
  toVertex 19 (Coord 0 18) === Just "A1"
  toVertex 19 (Coord 18 0) === Just "T19"
  toVertex 19 (Coord 3 15) === Just "D4"
  fromVertex 19 "D4" === Just (Coord 3 15)
  fromVertex 19 "Q16" === Just (Coord 15 3)

prop_readsWhatGnuGoWrites :: Property
prop_readsWhatGnuGoWrites = withTests 1 . property $ do
  -- GNU Go answers in capitals, and the protocol is not case
  -- sensitive, so both have to be read.
  fromVertex 9 "f5" === Just (Coord 5 4)
  fromVertex 9 "F5" === Just (Coord 5 4)
  moveFromVertex 9 "PASS" === Just Pass
  moveFromVertex 9 "resign" === Just Resign
  moveFromVertex 9 "F5" === Just (Play (Coord 5 4))

prop_refusesANameThatIsNotAPoint :: Property
prop_refusesANameThatIsNotAPoint = withTests 1 . property $ do
  fromVertex 9 "I5" === Nothing
  fromVertex 9 "K1" === Nothing
  fromVertex 9 "A10" === Nothing
  fromVertex 9 "A0" === Nothing
  fromVertex 9 "" === Nothing
  fromVertex 9 "pass" === Nothing

prop_aPointThatIsNotOnTheBoardHasNoName :: Property
prop_aPointThatIsNotOnTheBoardHasNoName = withTests 1 . property $ do
  -- A name and a point on a board of a given width are two ways of
  -- saying the same thing, and either way round there are values on
  -- one side with nothing on the other.
  toVertex 9 (Coord 9 0) === Nothing
  toVertex 9 (Coord 0 9) === Nothing
  toVertex 9 (Coord (-1) 0) === Nothing
  toVertex 9 (Coord 0 (-1)) === Nothing
  moveToVertex 9 (Play (Coord 9 9)) === Nothing
  moveToVertex 9 Pass === Just "pass"
  moveToVertex 9 Resign === Just "pass"
  moveToVertex 9 (Play (Coord 5 4)) === Just "F5"

prop_aColumnHasALetterWhenThereIsAColumn :: Property
prop_aColumnHasALetterWhenThereIsAColumn = withTests 1 . property $ do
  columnLetter 9 0 === Just 'A'
  columnLetter 9 8 === Just 'J'
  columnLetter 9 9 === Nothing
  columnLetter 9 (-1) === Nothing

tests :: Group
tests = $$(discover)