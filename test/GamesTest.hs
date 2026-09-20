-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

-- | The games this player is already in, out of what the website says
-- about them.
module GamesTest
  ( tests
  )
where

import           Data.Aeson                     ( Value(..)
                                                , encode
                                                , object
                                                , toJSON
                                                , (.=)
                                                )
import qualified Data.ByteString.Lazy          as Lazy
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Hedgehog

import           Go.Types
import           Stones.Ogs.Games
import           Stones.Setup                   ( Against(..)
                                                , Ongoing(..)
                                                , Setup(..)
                                                , Speed(..)
                                                )

-- | One game, as online-go.com lists it. Word for word what it sent
-- about a real one, with the parts nothing here reads left out.
--
-- Note that the settings arrive as a string with more JSON inside it
-- rather than as an object. That is the server's doing, not a mistake
-- here.
listed :: Int -> Int -> Int -> Value
listed game width black = object
  [ "id" .= game
  , "name" .= ("Stones" :: Text)
  , "width" .= width
  , "height" .= width
  , "ranked" .= False
  , "black" .= black
  , "white" .= (605979 :: Int)
  , "ended" .= Null
  , "handicap" .= (0 :: Int)
  , "time_control" .= ("byoyomi" :: Text)
  , "time_control_parameters"
    .= ("{\"system\": \"byoyomi\", \"speed\": \"live\", \"main_time\": 600}" :: Text)
  , "players" .= object
    [ "black" .= object ["id" .= black, "username" .= ("lenzuru" :: Text)]
    , "white" .= object
      ["id" .= (605979 :: Int), "username" .= ("amybot-beginner" :: Text)]
    ]
  ]

-- | A page of them, as the website sends one.
page :: [Value] -> Lazy.ByteString
page found = encode (object ["count" .= length found, "results" .= found])

read' :: Int -> [Value] -> Either Text [Ongoing]
read' player found = ongoingFrom player (Lazy.toStrict (page found))

prop_aGameSaysWhoItIsAgainstAndWhichOneItIs :: Property
prop_aGameSaysWhoItIsAgainstAndWhichOneItIs = withTests 1 . property $ do
  case read' 1768491 [listed 90884965 9 1768491] of
    Left  why       -> annotate (Text.unpack why) >> failure
    Right [already] -> do
      already.with === "amybot-beginner"
      already.game === 90884965
      already.about.against === Online
      already.about.size === 9
      -- Which colour this player has in it, so the board is the right
      -- way round before the server says so.
      already.about.human === Black
      -- Taken down as a game against that player, so playing them
      -- again afterwards asks the same person.
      already.about.versus === "amybot-beginner"
      -- The settings arrive as a string with more JSON inside it, so
      -- they are read twice.
      already.about.speed === Live
    Right many -> annotateShow (length many) >> failure

prop_theOtherSideOfTheSameBoardIsTheOtherPlayer :: Property
prop_theOtherSideOfTheSameBoardIsTheOtherPlayer =
  withTests 1 . property $ do
    -- Asked for by the player with White in it, whose opponent is the
    -- one with Black.
    case read' 605979 [listed 90884965 9 1768491] of
      Right [already] -> do
        already.with === "lenzuru"
        already.about.human === White
      other -> annotateShow (fmap length other) >> failure

prop_everyGameOnThePageComesBack :: Property
prop_everyGameOnThePageComesBack = withTests 1 . property $ do
  let found = [listed 1 9 1768491, listed 2 13 1768491, listed 3 19 1768491]
  (map (.game) <$> read' 1768491 found) === Right [1, 2, 3]
  -- And an empty list is a player in no games, rather than a failure.
  read' 1768491 [] === Right []

prop_aBoardThisProgramDoesNotPlayOnIsLeftOut :: Property
prop_aBoardThisProgramDoesNotPlayOnIsLeftOut = withTests 1 . property $ do
  -- The rules here are written for a square board, so a game on one
  -- that is not square is left out rather than listed and then failed
  -- at when somebody opens it.
  let oblong = object
        [ "id" .= (7 :: Int)
        , "width" .= (19 :: Int)
        , "height" .= (13 :: Int)
        , "black" .= (1768491 :: Int)
        , "players" .= object
          [ "black" .= object ["username" .= ("lenzuru" :: Text)]
          , "white" .= object ["username" .= ("somebody" :: Text)]
          ]
        ]
  (map (.game) <$> read' 1768491 [oblong, listed 8 9 1768491]) === Right [8]

prop_aSpeedThatCannotBeReadIsTakenAsALiveGame :: Property
prop_aSpeedThatCannotBeReadIsTakenAsALiveGame = withTests 1 . property $ do
  -- What it says changes nothing about the game, which the server is
  -- already running. It is what the window shows, and live is what
  -- most games are.
  let odd' = object
        [ "id" .= (7 :: Int)
        , "width" .= (9 :: Int)
        , "height" .= (9 :: Int)
        , "black" .= (1768491 :: Int)
        , "time_control_parameters" .= ("not json at all" :: Text)
        , "players" .= object
          [ "black" .= object ["username" .= ("lenzuru" :: Text)]
          , "white" .= object ["username" .= ("somebody" :: Text)]
          ]
        ]
  (map (.about.speed) <$> read' 1768491 [odd']) === Right [Live]

prop_anAnswerThatIsNotAListSaysSo :: Property
prop_anAnswerThatIsNotAListSaysSo = withTests 1 . property $ do
  -- A page that carries no list is worth saying something about,
  -- rather than showing a player no games and letting them think they
  -- are in none.
  case ongoingFrom 1768491 (Lazy.toStrict (encode (object ["count" .= (0 :: Int)])))
    of
      Left  why -> assert (Text.isInfixOf "no list of games" why)
      Right _   -> annotate "it read a list that was not there" >> failure
  case ongoingFrom 1768491 "not json at all" of
    Left  why -> assert (Text.isInfixOf "online-go.com answered with" why)
    Right _   -> annotate "it read something out of nothing" >> failure

prop_theAddressAsksForGamesThatHaveNotEnded :: Property
prop_theAddressAsksForGamesThatHaveNotEnded = withTests 1 . property $ do
  -- A player has hundreds of finished games and a handful of going
  -- ones, and the going ones are the whole point.
  let asked = address 1768491
  assert (Text.isInfixOf "/players/1768491/games/" asked)
  assert (Text.isInfixOf "ended__isnull=true" asked)
  assert (Text.isInfixOf "page_size=" asked)

-- | Silence the unused import warning for toJSON, which the fixtures
-- above do not name.
_unused :: Value
_unused = toJSON (0 :: Int)

tests :: Group
tests = $$(discover)
