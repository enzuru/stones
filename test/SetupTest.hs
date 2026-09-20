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
import           Stones.Setup

-- | Any of the choices the page offers.
choice :: Gen SetupEvent
choice = Gen.choice
  [ ChosePlace <$> Gen.element places
  , ChoseSpeed <$> Gen.element speeds
  , ChoseRanked <$> Gen.element [False, True]
  , ChoseHandicapping <$> Gen.element handicappings
  , ChoseBoard <$> Gen.element boardWidths
  , ChoseSide <$> Gen.element [Black, White]
  , ChoseStrength <$> Gen.element strengths
  , ChoseHandicap <$> Gen.element handicaps
  , pure StartPressed
  , pure SignInPressed
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
    === Setup { against = Computer, size = 19, human = White, strength = Fierce, handicap = 0, speed = Live, ranked = False, handicapping = Standard, versus = "" }
  chose defaultSetup (ChoseStrength Gentle) === defaultSetup { strength = Gentle }
  chose defaultSetup (ChosePlace Online) === defaultSetup { against = Online }
  chose defaultSetup (ChoseSpeed Blitz) === defaultSetup { speed = Blitz }
  chose defaultSetup (ChoseRanked True) === defaultSetup { ranked = True }
  chose defaultSetup (ChoseHandicapping Required)
    === defaultSetup { handicapping = Required }
  -- Signing in is not a choice about the game. It is about the window,
  -- and the page holds what it held.
  chose defaultSetup SignInPressed === defaultSetup

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
  assert (after.against `elem` places)
  assert (after.speed `elem` speeds)

prop_thePageSaysWhatItIsAskingFor :: Property
prop_thePageSaysWhatItIsAskingFor = withTests 1 . property $ do
  describeSetup defaultSetup === "19\215\&19  \183  Black  \183  Fierce"
  describeSetup
      Setup { against = Computer, size = 9, human = White, strength = Gentle, handicap = 0, speed = Live, ranked = False, handicapping = Standard, versus = "" }
    === "9\215\&9  \183  White  \183  Gentle"

prop_aPageAskingAboutAServerSaysNothingAboutAnEngine :: Property
prop_aPageAskingAboutAServerSaysNothingAboutAnEngine =
  withTests 1 . property $ do
    -- A game somebody else pairs settles the colour and the opponent
    -- on the other side, so naming either here would be saying
    -- something this program does not know yet.
    let online = defaultSetup { against = Online }
    describeSetup online === "19\215\&19  \183  online-go.com  \183  Live"
    describeSetup online { size = 9, human = White, strength = Gentle }
      === "9\215\&9  \183  online-go.com  \183  Live"
    -- What it does say is what that place was asked for.
    describeSetup online { speed = Blitz, ranked = True, versus = "amybot" }
      === "19\215\&19  \183  online-go.com  \183  Blitz  \183  Ranked  \183  amybot"
    -- A handicap asked of a server is a preference rather than a
    -- number, and standard is what it does anyway, so it says nothing.
    describeSetup online { handicapping = Standard }
      === "19\215\&19  \183  online-go.com  \183  Live"
    describeSetup online { handicapping = Disabled }
      === "19\215\&19  \183  online-go.com  \183  Live  \183  Disabled handicap"
    describeSetup online { handicapping = Required }
      === "19\215\&19  \183  online-go.com  \183  Live  \183  Required handicap"

prop_everySpeedHasOneNameHereAndAnotherOnTheWire :: Property
prop_everySpeedHasOneNameHereAndAnotherOnTheWire =
  withTests 1 . property $ do
    -- The server's word and the player's word are not always the same
    -- word. Nobody calls a correspondence game a correspondence game.
    map describeSpeed speeds === ["Blitz", "Rapid", "Live", "Daily"]
    map speedName speeds === ["blitz", "rapid", "live", "correspondence"]
    length speeds === 4

prop_aHandicapAskedOfAServerIsAPreference :: Property
prop_aHandicapAskedOfAServerIsAPreference = withTests 1 . property $ do
  -- Nobody picks how many stones a game somebody else is paired into
  -- starts with. The ranks decide that, and the server says where the
  -- game starts when it is joined.
  handicappings === [Required, Standard, Disabled]
  map describeHandicapping handicappings
    === ["Required", "Standard", "Disabled"]
  -- Whatever the two ranks call for, which is what most people want.
  defaultSetup.handicapping === Standard

prop_nothingCountsTowardsARankUnlessItIsAskedFor :: Property
prop_nothingCountsTowardsARankUnlessItIsAskedFor =
  withTests 1 . property $ do
    -- A rank belongs to the person who earned it, and a program plays
    -- some of these games.
    defaultSetup.ranked === False
    defaultSetup.speed === Live

prop_thereAreTwoPlacesToPlayAndBothHaveNames :: Property
prop_thereAreTwoPlacesToPlayAndBothHaveNames = withTests 1 . property $ do
  places === [Computer, Online]
  map describePlace places === ["This computer", "online-go.com"]
  -- Nothing is started until there is somebody to start it as, so a
  -- game on this machine is always ready and one on a server is not.
  defaultSetup.against === Computer

prop_signingInSaysWhereItHasGotTo :: Property
prop_signingInSaysWhereItHasGotTo = withTests 1 . property $ do
  describeSigning SignedOut === "Not signed in"
  describeSigning SigningIn === "Waiting for the browser\8230"
  describeSigning (SignedIn "hikaru") === "Signed in as hikaru"
  -- A refusal says why in the same place, rather than somewhere the
  -- player has to go looking for it.
  describeSigning (SignInRefused "the browser never came back")
    === "the browser never came back"
  map signedIn [SignedOut, SigningIn, SignedIn "hikaru", SignInRefused "no"]
    === [False, False, True, False]

prop_theStrengthsHaveNamesOfTheirOwn :: Property
prop_theStrengthsHaveNamesOfTheirOwn = withTests 1 . property $ do
  map describeStrength strengths === ["Gentle", "Fair", "Fierce"]
  length strengths === 3

prop_aHandicapIsNoneOrAtLeastTwo :: Property
prop_aHandicapIsNoneOrAtLeastTwo = withTests 1 . property $ do
  -- One stone is a move rather than a handicap, so it is not offered.
  handicaps === [0, 2, 3, 4, 5]
  assert (1 `notElem` handicaps)
  describeHandicap 0 === "None"
  describeHandicap 4 === "4"

prop_choosingAHandicapSticks :: Property
prop_choosingAHandicapSticks = withTests 1 . property $ do
  (chose defaultSetup (ChoseHandicap 4)).handicap === 4
  (chose defaultSetup (ChoseHandicap 0)).handicap === 0
  -- And it changes nothing else.
  (chose defaultSetup (ChoseHandicap 4)).size === defaultSetup.size
  (chose defaultSetup (ChoseHandicap 4)).human === defaultSetup.human

prop_aHandicapIsSaidOnlyWhenThereIsOne :: Property
prop_aHandicapIsSaidOnlyWhenThereIsOne = withTests 1 . property $ do
  describeSetup defaultSetup === "19\215\&19  \183  Black  \183  Fierce"
  describeSetup defaultSetup { handicap = 4 }
    === "19\215\&19  \183  Black  \183  Fierce  \183  4 stones"

tests :: Group
tests = $$(discover)