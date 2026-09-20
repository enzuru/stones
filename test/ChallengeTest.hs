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
                                                , Handicapping(..)
                                                , Setup(..)
                                                , Speed(..)
                                                , Strength(..)
                                                , handicappings
                                                , speedName
                                                , speeds
                                                )

-- | A game to ask for.
wanted :: Setup
wanted =
  Setup { against = Computer, size = 9, human = Black, strength = Strength 1, handicap = 0, speed = Live, ranked = False, handicapping = Standard, versus = "" }

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

prop_theClockFollowsTheSpeedThatWasAskedFor :: Property
prop_theClockFollowsTheSpeedThatWasAskedFor = property $ do
  how <- forAll (Gen.element speeds)
  let clock = ofGame wanted { speed = how } "time_control_parameters"
      whole key = case clock >>= field key of
        Just (Number n) -> Just (round n :: Int)
        _               -> Nothing
      think = whole "main_time"
      each  = whole "period_time"
  -- The speed the server was asked for is the speed it is told.
  (clock >>= field "speed") === Just (String (speedName how))
  -- A quicker game has a shorter clock, in both halves of it, and a
  -- daily game is measured in days.
  case how of
    Blitz          -> (think, each) === (Just 30, Just 10)
    Rapid          -> (think, each) === (Just 120, Just 20)
    Live           -> (think, each) === (Just 600, Just 60)
    Correspondence -> (think, each) === (Just (3 * 86400), Just 86400)

prop_aGameCountsTowardsARankOnlyWhenItIsAskedTo :: Property
prop_aGameCountsTowardsARankOnlyWhenItIsAskedTo =
  withTests 1 . property $ do
    -- A rank belongs to the person who earned it, and a program plays
    -- some of these games, so it has to be asked for.
    ofGame wanted "ranked" === Just (Bool False)
    ofGame wanted { ranked = True } "ranked" === Just (Bool True)

prop_theBoardIsTheOneThatWasChosen :: Property
prop_theBoardIsTheOneThatWasChosen = property $ do
  size <- forAll (Gen.element [9, 13, 19])
  let setup = wanted { size = size }
  ofGame setup "width" === Just (Number (fromIntegral size))
  ofGame setup "height" === Just (Number (fromIntegral size))

prop_aHandicapOnAChallengeIsANumberTheServerWorksOut :: Property
prop_aHandicapOnAChallengeIsANumberTheServerWorksOut = property $ do
  how <- forAll (Gen.element handicappings)
  -- Asking one player is asking them: they take the game or they do
  -- not. So the nearest a challenge comes to asking for a handicap is
  -- minus one, which tells the server to work one out from the two
  -- ranks, and required and standard come to the same thing here.
  -- Only matchmaking can tell them apart.
  let stones = case how of
        Disabled -> 0 :: Int
        _        -> -1
  ofGame wanted { handicapping = how } "handicap"
    === Just (Number (fromIntegral stones))

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
