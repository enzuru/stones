-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later


-- | Looking for somebody to play against.
module SeekTest
  ( tests
  )
where

import           Data.Aeson                     ( Value(..)
                                                , object
                                                , (.=)
                                                )
import qualified Data.Aeson                    as Aeson
import qualified Data.Aeson.Key               as Key
import qualified Data.Aeson.Types             as Aeson
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Hedgehog
import qualified Hedgehog.Gen                  as Gen


import           Stones.Ogs.Frame               ( Packet(..) )
import           Stones.Ogs.Seek
import           Stones.Setup

-- | A 9x9 blitz game against anybody.
wanted :: Setup
wanted = defaultSetup { against  = Online
                      , size     = 9
                      , speed    = Blitz
                      , handicap = 0
                      }

-- | One that is being looked for.
looking :: Seek
looking = Seek { name = "a-name", wants = wanted }

-- | What the server says when it finds one.
found :: Text -> Int -> Value
found which game = object ["uuid" .= which, "game_id" .= game]

prop_askingSaysWhatKindOfGameIsWanted :: Property
prop_askingSaysWhatKindOfGameIsWanted = withTests 1 . property $ do
  finding looking
    === Event
          "automatch/find_match"
          [ object
              [ "uuid" .= ("a-name" :: Text)
              , "size_speed_options" .= ([ object
                      [ "size" .= ("9x9" :: Text)
                      , "speed" .= ("blitz" :: Text)
                      , "system" .= ("byoyomi" :: Text)
                      ]
                    ] :: [Value]
                  )
              , "lower_rank_diff" .= (3 :: Int)
              , "upper_rank_diff" .= (3 :: Int)
              , "rules" .= object
                [ "condition" .= ("preferred" :: Text)
                , "value" .= ("japanese" :: Text)
                ]
              , "handicap" .= object
                [ "condition" .= ("no-preference" :: Text)
                , "value" .= ("enabled" :: Text)
                ]
              ]
          ]
          Nothing

prop_aHandicapIsAskedForRatherThanCounted :: Property
prop_aHandicapIsAskedForRatherThanCounted = property $ do
  -- Nobody picks how many. The ranks decide that, and the server says
  -- where a game starts when it is joined, stones and all, so what is
  -- asked for here is whether to be paired into one at all.
  size <- forAll (Gen.element ([9, 13, 19] :: [Int]))
  fast <- forAll (Gen.element speeds)
  how  <- forAll (Gen.element handicappings)
  let asked = Seek
        { name  = "n"
        , wants = wanted { size = size, speed = fast, handicapping = how }
        }
      -- Standard is what the server does when nobody says otherwise,
      -- so it is a preference. The other two are insisted on, because
      -- what they are for is ruling the other kind of game out.
      (condition, want) = case how of
        Required -> ("required", "enabled")
        Standard -> ("no-preference", "enabled")
        Disabled -> ("required", "disabled")
  case finding asked of
    Event _ [body] _ -> assert (asks body "handicap" condition want)
    other            -> annotateShow other >> failure

prop_everyBoardAndSpeedHasAName :: Property
prop_everyBoardAndSpeedHasAName = withTests 1 . property $ do
  map speedName speeds === ["blitz", "rapid", "live", "correspondence"]
  case finding looking { wants = wanted { size = 19 } } of
    Event _ [body] _ -> assert (mentions body "19x19")
    other            -> annotateShow other >> failure
  case finding looking { wants = wanted { size = 13, speed = Correspondence } } of
    Event _ [body] _ -> do
      assert (mentions body "13x13")
      assert (mentions body "correspondence")
    other -> annotateShow other >> failure

prop_stoppingNamesWhatToStop :: Property
prop_stoppingNamesWhatToStop = withTests 1 . property $ do
  cancelling looking
    === Event "automatch/cancel" [object ["uuid" .= ("a-name" :: Text)]] Nothing

prop_theAnswerCarriesTheGameToJoin :: Property
prop_theAnswerCarriesTheGameToJoin = withTests 1 . property $ do
  started looking "automatch/start" [found "a-name" 1234] === Just 1234

prop_somebodyElsesAnswerIsNotOurs :: Property
prop_somebodyElsesAnswerIsNotOurs = withTests 1 . property $ do
  -- The answer names the looking it answers rather than who did it,
  -- and one connection carries every looking at once.
  started looking "automatch/start" [found "another-name" 1234] === Nothing
  started looking "automatch/entry" [found "a-name" 1234] === Nothing
  started looking "automatch/start" [] === Nothing
  started looking "automatch/start" [object ["uuid" .= ("a-name" :: Text)]]
    === Nothing
  started looking "automatch/start" [String "a-name"] === Nothing

prop_twoLookingsAreNotTheSameLooking :: Property
prop_twoLookingsAreNotTheSameLooking = property $ do
  one   <- evalIO (newSeek wanted)
  other <- evalIO (newSeek wanted)
  assert (one.name /= other.name)
  -- And each one is looking for what it was asked to look for.
  one.wants === wanted
  other.wants === wanted
  -- Eight, four, four, four and twelve, with the dashes between them.
  Text.length one.name === 36
  Text.count "-" one.name === 4

-- | Whether a payload asks for something under a condition.
asks :: Value -> Text -> Text -> Text -> Bool
asks body key condition value = case body of
  Object fields -> case lookupKey fields key of
    Just (Object inner) ->
      lookupKey inner "condition"
        == Just (String condition)
        && lookupKey inner "value"
        == Just (String value)
    _ -> False
  _ -> False

-- | Whether this text appears anywhere in a payload.
mentions :: Value -> Text -> Bool
mentions body word = Text.isInfixOf word (Text.pack (show body))

-- | A field of an object, whatever aeson calls its keys this week.
lookupKey :: Aeson.Object -> Text -> Maybe Value
lookupKey fields key = Aeson.parseMaybe (Aeson..: Key.fromText key) fields

tests :: Group
tests = $$(discover)
