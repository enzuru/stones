-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

-- | What a game is started from.
module SetupTest
  ( tests
  )
where

import           Hedgehog
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range

import           Go.Types
import           Stones.Engine
import           Stones.Setup

-- | Any of the choices the page offers.
choice :: Gen SetupEvent
choice = Gen.choice
  [ ChoseBoard <$> Gen.element boardWidths
  , ChoseSide <$> Gen.element [Black, White]
  , ChoseStrength <$> Gen.element strengths
  , pure StartPressed
  ]

prop_aGameStartsAsAGameOfGoDoes :: Property
prop_aGameStartsAsAGameOfGoDoes = withTests 1 . property $ do
  -- A full board, the colour that moves first, and an opponent that
  -- is trying, which is what somebody who says nothing has asked for.
  defaultSetup.size === 19
  defaultSetup.human === Black
  defaultSetup.strength === Fierce
  assert (defaultSetup.size `elem` boardWidths)

prop_theBoardsOnOfferAreTheOnesPeoplePlay :: Property
prop_theBoardsOnOfferAreTheOnesPeoplePlay = withTests 1 . property $ do
  boardWidths === [9, 13, 19]

prop_aChoiceChangesTheOneThingItIsAbout :: Property
prop_aChoiceChangesTheOneThingItIsAbout = withTests 1 . property $ do
  chose defaultSetup (ChoseBoard 9) === defaultSetup { size = 9 }
  chose defaultSetup (ChoseSide White)
    === Setup { size = 19, human = White, strength = Fierce }
  chose defaultSetup (ChoseStrength Gentle) === defaultSetup { strength = Gentle }

prop_startingIsNotAChoice :: Property
prop_startingIsNotAChoice = property $ do
  -- The button is the window's business. The page holds what was
  -- chosen and pressing it chooses nothing.
  made <- forAll (Gen.list (Range.linear 0 8) choice)
  let after = foldl chose defaultSetup made
  chose after StartPressed === after

prop_everyChoiceSticks :: Property
prop_everyChoiceSticks = property $ do
  made <- forAll (Gen.list (Range.linear 1 8) choice)
  let after = foldl chose defaultSetup made
  -- Whatever was chosen last about a thing is what the page is asking
  -- for, and nothing else about it moved.
  assert (after.size `elem` boardWidths)
  assert (after.strength `elem` strengths)

prop_thePageSaysWhatItIsAskingFor :: Property
prop_thePageSaysWhatItIsAskingFor = withTests 1 . property $ do
  describeSetup defaultSetup === "19\215\&19  \183  Black  \183  Fierce"
  describeSetup Setup { size = 9, human = White, strength = Gentle }
    === "9\215\&9  \183  White  \183  Gentle"

prop_theStrengthsHaveNamesOfTheirOwn :: Property
prop_theStrengthsHaveNamesOfTheirOwn = withTests 1 . property $ do
  map describeStrength strengths === ["Gentle", "Fair", "Fierce"]
  length strengths === 3

tests :: Group
tests = $$(discover)
