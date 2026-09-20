-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | What this program says to online-go.com about a game, and what it
-- makes of what comes back.
--
-- None of this touches a socket. What is checked here is the shape of
-- what goes on one.
module ProtocolMessageTest
  ( tests
  )
where

import           Data.Aeson                     ( Value(..)
                                                , object
                                                , toJSON
                                                , (.=)
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Hedgehog
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range

import           Go.Types
import           Stones.Ogs.Protocol
import           Stones.Opponent                ( Heard(..)
                                                , Told(..)
                                                )

-- | The game these are all about.
table :: Table
table = Table { identifier = 1234, played = 10 }

-- | A point on a board that size.
point :: Int -> Gen Coord
point size = Coord <$> Gen.int (Range.linear 0 (size - 1)) <*> Gen.int
  (Range.linear 0 (size - 1))

-- | A move as it arrives: a column, a row, and how long it took.
arriving :: Int -> Int -> Value
arriving x y = toJSON [x, y, 0 :: Int]

-- | The payload of a move that arrived.
moveEvent :: Int -> Value -> Value
moveEvent number move =
  object ["game_id" .= (1234 :: Int), "move_number" .= number, "move" .= move]

prop_aMoveIsTwoLettersBothCountedFromA :: Property
prop_aMoveIsTwoLettersBothCountedFromA = withTests 1 . property $ do
  encodeMove (Play (Coord 0 0)) === "aa"
  encodeMove (Play (Coord 3 3)) === "dd"
  -- The far corner of the biggest board anybody plays on.
  encodeMove (Play (Coord 18 18)) === "ss"
  encodeMove (Play (Coord 0 18)) === "as"
  encodeMove (Play (Coord 18 0)) === "sa"

prop_aPassIsTwoFullStops :: Property
prop_aPassIsTwoFullStops = withTests 1 . property $ do
  encodeMove Pass === ".."
  -- Giving the game up is not a move and has an event of its own, so
  -- there is nothing better to write here than the same thing.
  encodeMove Resign === ".."

prop_everyPointOnTheBoardHasAWritingOfItsOwn :: Property
prop_everyPointOnTheBoardHasAWritingOfItsOwn = property $ do
  here  <- forAll (point 19)
  there <- forAll (point 19)
  if here == there
    then encodeMove (Play here) === encodeMove (Play there)
    else assert (encodeMove (Play here) /= encodeMove (Play there))
  Text.length (encodeMove (Play here)) === 2

prop_aMoveThatArrivesIsTheColumnAndTheRow :: Property
prop_aMoveThatArrivesIsTheColumnAndTheRow = property $ do
  Coord x y <- forAll (point 19)
  decodeMove (arriving x y) === Right (Play (Coord x y))

prop_aMoveOffTheBoardIsAPass :: Property
prop_aMoveOffTheBoardIsAPass = withTests 1 . property $ do
  decodeMove (arriving (-1) (-1)) === Right Pass
  -- And what is not a move at all is refused rather than guessed at.
  assert (isLeft (decodeMove (toJSON ([] :: [Int]))))
  assert (isLeft (decodeMove (toJSON ("dd" :: Text))))
  assert (isLeft (decodeMove (toJSON [Null])))

prop_whoMovedIsWorkedOutFromTheCount :: Property
prop_whoMovedIsWorkedOutFromTheCount = withTests 1 . property $ do
  -- The server says which move it was and not who made it. Black goes
  -- first, so the count decides.
  moverOf 0 === Black
  moverOf 1 === White
  moverOf 2 === Black
  moverOf 99 === White

prop_aMoveIsSentAsItsOwnEvent :: Property
prop_aMoveIsSentAsItsOwnEvent = withTests 1 . property $ do
  outgoing table (Moved (Play (Coord 3 3)))
    === [ ( "game/move"
          , object ["game_id" .= (1234 :: Int), "move" .= ("dd" :: Text)]
          )
        ]

prop_givingUpIsNotAMove :: Property
prop_givingUpIsNotAMove = withTests 1 . property $ do
  outgoing table Resigned
    === [("game/resign", object ["game_id" .= (1234 :: Int)])]

prop_somethingSaidCarriesWhereTheGameWas :: Property
prop_somethingSaidCarriesWhereTheGameWas = withTests 1 . property $ do
  outgoing table (Said "good game")
    === [ ( "game/chat"
          , object
            [ "game_id" .= (1234 :: Int)
            , "type" .= ("main" :: Text)
            , "move_number" .= (10 :: Int)
            , "body" .= ("good game" :: Text)
            ]
          )
        ]

prop_takingBackAsksForItRatherThanDoingIt :: Property
prop_takingBackAsksForItRatherThanDoingIt = withTests 1 . property $ do
  -- The other player has to agree, so this is a request and what comes
  -- back is what decides.
  outgoing table (TookBack 2)
    === [ ( "game/undo/request"
          , object ["game_id" .= (1234 :: Int), "move_number" .= (8 :: Int)]
          )
        ]
  -- And it never asks to go back past the start.
  outgoing Table { identifier = 1, played = 1 } (TookBack 9)
    === [("game/undo/request", object ["game_id" .= (1 :: Int), "move_number" .= (0 :: Int)])]

prop_whatTheServerNeedsNoTellingOfIsNotSent :: Property
prop_whatTheServerNeedsNoTellingOfIsNotSent = withTests 1 . property $ do
  -- The server keeps the rules, so it knows whose turn it is, and it
  -- runs the counting at the end itself.
  outgoing table Asked === []
  outgoing table Counting === []

prop_aMoveArrivesAsAMoveWithAColourOnIt :: Property
prop_aMoveArrivesAsAMoveWithAColourOnIt = withTests 1 . property $ do
  incoming table "game/1234/move" [moveEvent 0 (arriving 3 3)]
    === Just (Played Black (Play (Coord 3 3)))
  incoming table "game/1234/move" [moveEvent 1 (arriving 15 3)]
    === Just (Played White (Play (Coord 15 3)))
  incoming table "game/1234/move" [moveEvent 4 (arriving (-1) (-1))]
    === Just (Played Black Pass)

prop_aMoveAboutAnotherGameIsNotAboutThisOne :: Property
prop_aMoveAboutAnotherGameIsNotAboutThisOne = withTests 1 . property $ do
  -- One socket carries every game at once, so the name is the only
  -- thing that says which game an event belongs to.
  incoming table "game/9999/move" [moveEvent 0 (arriving 3 3)] === Nothing
  incoming table "game/move" [moveEvent 0 (arriving 3 3)] === Nothing

prop_aMoveThatCannotBeReadLosesTheGameRatherThanBeingDropped :: Property
prop_aMoveThatCannotBeReadLosesTheGameRatherThanBeingDropped =
  withTests 1 . property $ do
    -- Dropping it would leave this program a move behind the server
    -- for the rest of the game, and never say so.
    case incoming table "game/1234/move" [moveEvent 0 (toJSON ("dd" :: Text))] of
      Just (Lost why) -> assert (not (Text.null why))
      other           -> annotateShow other >> failure

prop_somethingSaidArrivesWithWhoSaidIt :: Property
prop_somethingSaidArrivesWithWhoSaidIt = withTests 1 . property $ do
  incoming
      table
      "game/1234/chat"
      [ object
          [ "channel" .= ("main" :: Text)
          , "line" .= object
            [ "username" .= ("someone" :: Text)
            , "body" .= ("good game" :: Text)
            , "move_number" .= (10 :: Int)
            ]
          ]
      ]
    === Just (Spoke "someone" "good game")

prop_anUndoTheOtherPlayerAgreedToComesBackAsMovesOff :: Property
prop_anUndoTheOtherPlayerAgreedToComesBackAsMovesOff =
  withTests 1 . property $ do
    incoming table "game/1234/undo_accepted" [toJSON (8 :: Int)]
      === Just (Undid 2)

prop_whatThisProgramDoesNotActOnIsNothingToSay :: Property
prop_whatThisProgramDoesNotActOnIsNothingToSay = withTests 1 . property $ do
  -- The clock and the counting at the end are both still to come, and
  -- until they are, an event about them is quietly none of this
  -- program's business.
  incoming table "game/1234/clock" [object []] === Nothing
  incoming table "game/1234/phase" [String "play"] === Nothing
  incoming table "game/1234/removed_stones" [object []] === Nothing
  incoming table "game/1234/move" [] === Nothing

isLeft :: Either a b -> Bool
isLeft = either (const True) (const False)

tests :: Group
tests = $$(discover)
