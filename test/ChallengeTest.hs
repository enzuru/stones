-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

-- | Challenging somebody in particular.
--
-- The two calls are the only parts that leave this machine and neither
-- is here. What is here is what gets asked for and what is made of the
-- answer.
module ChallengeTest
  ( tests
  )
where

import           Data.Aeson                     ( Value(..) )
import qualified Data.Aeson                    as Aeson
import qualified Data.Aeson.Key                as Key
import qualified Data.Aeson.Types              as Aeson
import           Data.Text                      ( Text )
import qualified Data.Text.Encoding            as Text
import           Hedgehog
import qualified Hedgehog.Gen                  as Gen

import           Go.Types
import           Stones.Ogs.Challenge
import           Stones.Setup                   ( Against(..)
                                                , Setup(..)
                                                , Strength(..)
                                                , handicaps
                                                )

-- | A game to ask for.
wanted :: Setup
wanted =
  Setup { against = Computer, size = 9, human = Black, strength = Gentle, handicap = 0, versus = "" }

-- | A field of what is asked for.
askedFor :: Setup -> Text -> Maybe Value
askedFor setup key = field key (asking setup)

-- | A field of the game inside it.
ofGame :: Setup -> Text -> Maybe Value
ofGame setup key = field "game" (asking setup) >>= field key

field :: Text -> Value -> Maybe Value
field key = \value -> case value of
  Object fields -> Aeson.parseMaybe (Aeson..: Key.fromText key) fields
  _             -> Nothing

prop_aChallengeIsNeverRanked :: Property
prop_aChallengeIsNeverRanked = property $ do
  -- A rank belongs to whoever earned it, and these are games played by
  -- a program to find out whether it works.
  size  <- forAll (Gen.element [9, 13, 19])
  side  <- forAll (Gen.element [Black, White])
  stone <- forAll (Gen.element handicaps)
  let setup = wanted { size = size, human = side, handicap = stone }
  ofGame setup "ranked" === Just (Bool False)

prop_theClockIsLongBecauseNobodyWasAskedWhatItShouldBe :: Property
prop_theClockIsLongBecauseNobodyWasAskedWhatItShouldBe =
  withTests 1 . property $ do
    -- A time nobody loses to by accident, and one the other side will
    -- take: a program asked for a game usually has a limit on how long
    -- a game it will sit through.
    let clock = ofGame wanted "time_control_parameters"
    (clock >>= field "main_time") === Just (Number 600)
    (clock >>= field "period_time") === Just (Number 60)
    (clock >>= field "periods") === Just (Number 5)

prop_theBoardAndTheStonesAreWhatWasChosen :: Property
prop_theBoardAndTheStonesAreWhatWasChosen = property $ do
  size  <- forAll (Gen.element [9, 13, 19])
  stone <- forAll (Gen.element handicaps)
  let setup = wanted { size = size, handicap = stone }
  ofGame setup "width" === Just (Number (fromIntegral size))
  ofGame setup "height" === Just (Number (fromIntegral size))
  ofGame setup "handicap" === Just (Number (fromIntegral stone))

prop_theColourAskedForIsTheOneChosen :: Property
prop_theColourAskedForIsTheOneChosen = withTests 1 . property $ do
  askedFor wanted { human = Black } "challenger_color" === Just (String "black")
  askedFor wanted { human = White } "challenger_color" === Just (String "white")

prop_theRulesAreTheOnesTheseRulesAgreeWith :: Property
prop_theRulesAreTheOnesTheseRulesAgreeWith = withTests 1 . property $ do
  -- Japanese settles a repeated position the way this program's own
  -- rules do.
  ofGame wanted "rules" === Just (String "japanese")

prop_theGameToJoinIsReadOutOfTheAnswer :: Property
prop_theGameToJoinIsReadOutOfTheAnswer = withTests 1 . property $ do
  startedFrom
      (Text.encodeUtf8 "{\"challenge\":55,\"game\":1234,\"uuid\":\"abc\"}")
    === Right Started { challenge = 55, game = 1234 }
  assert (isLeft (startedFrom (Text.encodeUtf8 "{\"challenge\":55}")))
  assert (isLeft (startedFrom (Text.encodeUtf8 "not json")))

prop_somebodyIsFoundByTheirName :: Property
prop_somebodyIsFoundByTheirName = withTests 1 . property $ do
  playerFrom
      (Text.encodeUtf8
        "{\"count\":1,\"results\":[{\"id\":1234,\"username\":\"a-bot\"}]}"
      )
    === Right 1234
  -- Nobody by that name is a thing to say rather than a thing to
  -- guess at.
  assert (isLeft (playerFrom (Text.encodeUtf8 "{\"count\":0,\"results\":[]}")))
  assert (isLeft (playerFrom (Text.encodeUtf8 "{}")))
  assert (isLeft (playerFrom (Text.encodeUtf8 "not json")))

isLeft :: Either a b -> Bool
isLeft = either (const True) (const False)

tests :: Group
tests = $$(discover)
