-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
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
import qualified Data.Aeson.Key                as Key
import qualified Data.Aeson.KeyMap             as KeyMap
import           Stones.Opponent                ( Clock(..)
                                                , Heard(..)
                                                , Opening(..)
                                                , Time(..)
                                                , Told(..)
                                                )

-- | The game these are all about.
table :: Table
table = Table { identifier = 1234, played = 10, opens = Black, me = 77 }

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

prop_whoMovedIsWorkedOutFromWhoOpened :: Property
prop_whoMovedIsWorkedOutFromWhoOpened = withTests 1 . property $ do
  -- The server says which move it was and not who made it, so who
  -- opened and the count decide between them.
  moverOf Black 0 === Black
  moverOf Black 1 === White
  moverOf Black 2 === Black
  moverOf Black 99 === White
  -- A handicap game has Black's stones down before anybody has moved,
  -- so White is the one who moves first and every colour after it
  -- follows from that.
  moverOf White 0 === White
  moverOf White 1 === Black

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
  outgoing table { identifier = 1, played = 1 } (TookBack 9)
    === [ ( "game/undo/request"
          , object ["game_id" .= (1 :: Int), "move_number" .= (0 :: Int)]
          )
        ]

prop_whatTheServerNeedsNoTellingOfIsNotSent :: Property
prop_whatTheServerNeedsNoTellingOfIsNotSent = withTests 1 . property $ do
  -- The server keeps the rules, so it knows whose turn it is, and it
  -- runs the counting at the end itself.
  outgoing table Asked === []
  outgoing table Counting === []

prop_aMoveArrivesAsAMoveWithAColourOnIt :: Property
prop_aMoveArrivesAsAMoveWithAColourOnIt = withTests 1 . property $ do
  -- The server counts the moves rather than numbering them from
  -- nothing, so the first move of the game is number one.
  incoming table "game/1234/move" [moveEvent 1 (arriving 3 3)]
    === Just (Played Black (Play (Coord 3 3)))
  incoming table "game/1234/move" [moveEvent 2 (arriving 15 3)]
    === Just (Played White (Play (Coord 15 3)))
  incoming table "game/1234/move" [moveEvent 5 (arriving (-1) (-1))]
    === Just (Played Black Pass)

prop_theFirstMoveOfAGameBelongsToWhoeverOpenedIt :: Property
prop_theFirstMoveOfAGameBelongsToWhoeverOpenedIt =
  withTests 1 . property $ do
    -- Word for word what online-go.com sent on the first move of the
    -- first game this program ever played there. Read as a number
    -- counted from nothing rather than as a count, it made the move
    -- White's: the player's own stone came back as the other side's,
    -- the rules refused a stone on a point that already had one, and
    -- the game stopped on move one.
    let asSent = object
          [ "game_id" .= (1234 :: Int)
          , "move_number" .= (1 :: Int)
          , "move" .= toJSON [4, 4, 5592 :: Int]
          ]
    incoming table "game/1234/move" [asSent]
      === Just (Played Black (Play (Coord 4 4)))
    -- And in a handicap game, where White is the one who opens.
    let handicap = Table { identifier = 1234
                         , played     = 10
                         , opens      = White
                         , me         = 77
                         }
    incoming handicap "game/1234/move" [asSent]
      === Just (Played White (Play (Coord 4 4)))

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

prop_whatAProgramSaysArrivesAsSomethingToBeTranslated :: Property
prop_whatAProgramSaysArrivesAsSomethingToBeTranslated =
  withTests 1 . property $ do
    -- Word for word what amybot-beginner said at the start of the
    -- first game this program played. A person's words arrive as they
    -- were typed; a program's arrive as what it meant, under the name
    -- of each language it knows. Reading only the first kind threw
    -- away every hello and every good game.
    let saidByABot = object
          [ "channel" .= ("main" :: Text)
          , "line" .= object
            [ "body" .= object
              [ "en" .= ("Good luck, have fun!" :: Text)
              , "type" .= ("translated" :: Text)
              ]
            , "move_number" .= Null
            , "player_id" .= (605979 :: Int)
            , "username" .= ("amybot-beginner" :: Text)
            ]
          ]
    incoming table "game/1234/chat" [saidByABot]
      === Just (Spoke "amybot-beginner" "Good luck, have fun!")
    -- One with nothing anybody here can read is nothing to say, rather
    -- than a line with a blank in it.
    let unreadable = object
          [ "line" .= object
            [ "body" .= object ["type" .= ("translated" :: Text)]
            , "username" .= ("amybot-beginner" :: Text)
            ]
          ]
    incoming table "game/1234/chat" [unreadable] === Nothing

prop_theServerSayingToForgetIsWorthActingOn :: Property
prop_theServerSayingToForgetIsWorthActingOn = withTests 1 . property $ do
  -- A game ending sends this and then says the whole conversation over
  -- again. Ignoring it showed every line twice.
  incoming table "game/1234/reset-chats" [toJSON (0 :: Int)] === Just Hushed
  -- And it is about one game, like everything else.
  incoming table "game/9999/reset-chats" [toJSON (0 :: Int)] === Nothing

prop_anUndoTheOtherPlayerAgreedToComesBackAsMovesOff :: Property
prop_anUndoTheOtherPlayerAgreedToComesBackAsMovesOff =
  withTests 1 . property $ do
    incoming table "game/1234/undo_accepted" [toJSON (8 :: Int)]
      === Just (Undid 2)

prop_whatThisProgramDoesNotActOnIsNothingToSay :: Property
prop_whatThisProgramDoesNotActOnIsNothingToSay = withTests 1 . property $ do
  -- The counting at the end is still to come, and until it is, an
  -- event about it is quietly none of this program's business. So is
  -- an event with nothing in it at all.
  incoming table "game/1234/phase" [String "play"] === Nothing
  incoming table "game/1234/removed_stones" [object []] === Nothing
  incoming table "game/1234/move" [] === Nothing

-- * The counting at the end
-----------------------------

prop_markingAGroupSaysWhichStonesAndWhetherTheyAreDead :: Property
prop_markingAGroupSaysWhichStonesAndWhetherTheyAreDead =
  withTests 1 . property $ do
    outgoing table (Marking [Coord 0 0, Coord 1 1] True)
      === [ ( "game/removed_stones/set"
            , object
              [ "game_id" .= (1234 :: Int)
              , "stones" .= ("aabb" :: Text)
              , "removed" .= True
              ]
            )
          ]
    -- And the same click again, bringing the group back to life.
    outgoing table (Marking [Coord 0 0] False)
      === [ ( "game/removed_stones/set"
            , object
              [ "game_id" .= (1234 :: Int)
              , "stones" .= ("aa" :: Text)
              , "removed" .= False
              ]
            )
          ]

prop_acceptingSaysTheWholeSetAndNotWhatChanged :: Property
prop_acceptingSaysTheWholeSetAndNotWhatChanged = withTests 1 . property $ do
  -- The game is counted once both sides have accepted the same set,
  -- so what is accepted is the set rather than a yes.
  outgoing table (Accepting [Coord 2 2, Coord 3 4])
    === [ ( "game/removed_stones/accept"
          , object
            [ "game_id" .= (1234 :: Int)
            , "stones" .= ("ccde" :: Text)
            , "strict_seki_mode" .= False
            ]
          )
        ]

prop_everySetOfStonesHasAWritingOfItsOwn :: Property
prop_everySetOfStonesHasAWritingOfItsOwn = property $ do
  stones <- forAll (Gen.list (Range.linear 0 20) (point 19))
  -- What is written is what comes back, which is what lets a set
  -- marked here be the set agreed to over there.
  decodeStones (encodeStones stones) === stones

prop_theGameStoppingIsNewsEvenBeforeAnythingIsMarked :: Property
prop_theGameStoppingIsNewsEvenBeforeAnythingIsMarked =
  withTests 1 . property $ do
    -- Both players passed, so nobody moves again. The marks come
    -- separately and may come first, so this says which part of the
    -- game it is and nothing about what is in it.
    incoming table "game/1234/phase" [String "stone removal"] === Just Stopped
    incoming table "game/1234/phase" [String "play"] === Nothing
    incoming table "game/1234/phase" [String "finished"] === Nothing

prop_theStonesMarkedDeadArriveAsTheWholeSet :: Property
prop_theStonesMarkedDeadArriveAsTheWholeSet = withTests 1 . property $ do
  let marked = object
        [ "removed" .= True
        , "stones" .= ("aa" :: Text)
        , "all_removed" .= ("aabbcc" :: Text)
        ]
  incoming table "game/1234/removed_stones" [marked]
    === Just (Removing [Coord 0 0, Coord 1 1, Coord 2 2])
  -- And a set emptied again, which is a group brought back to life.
  let none' = object ["removed" .= False, "all_removed" .= ("" :: Text)]
  incoming table "game/1234/removed_stones" [none'] === Just (Removing [])

prop_aChangeWithNoWholeSetBesideItIsNothingToActOn :: Property
prop_aChangeWithNoWholeSetBesideItIsNothingToActOn =
  withTests 1 . property $ do
    -- A set worked out from a change is a set that can be wrong, and a
    -- wrong one would have this player agreeing to a count they cannot
    -- see. Showing none is the safe way to be wrong.
    let change = object ["removed" .= True, "stones" .= ("aa" :: Text)]
    incoming table "game/1234/removed_stones" [change] === Nothing

prop_aFinishedGameComesBackAsWhatItCameToAndNotAsANewOne :: Property
prop_aFinishedGameComesBackAsWhatItCameToAndNotAsANewOne =
  withTests 1 . property $ do
    -- The server sends the whole of a game again when it ends. Reading
    -- that as a game starting would put the board back up and throw
    -- the result away.
    let over = object
          [ "phase" .= ("finished" :: Text)
          , "outcome" .= ("12.5 points" :: Text)
          , "winner" .= (77 :: Int)
          , "black_player_id" .= (77 :: Int)
          , "white_player_id" .= (99 :: Int)
          ]
    incoming table "game/1234/gamedata" [over] === Just (Counted "B+12.5 points")
    -- The same game won by the other side.
    let theirs = replacing "winner" (toJSON (99 :: Int)) over
    incoming table "game/1234/gamedata" [theirs]
      === Just (Counted "W+12.5 points")
    -- A game still being played is still a game starting.
    resultFrom (object ["phase" .= ("play" :: Text)]) === Nothing

-- * The clock
---------------

-- | What the server says about the clocks of a game with a time to
-- think in and five short periods after it, with Black to play.
--
-- 77 is the player at this window, and Black in this game.
clockEvent :: Value
clockEvent = object
  [ "game_id" .= (1234 :: Int)
  , "current_player" .= (77 :: Int)
  , "black_player_id" .= (77 :: Int)
  , "white_player_id" .= (99 :: Int)
  , "black_time" .= object
    ["thinking_time" .= (301.5 :: Double), "periods" .= (5 :: Int), "period_time" .= (30 :: Int)]
  , "white_time" .= object
    ["thinking_time" .= (60 :: Int), "periods" .= (5 :: Int), "period_time" .= (30 :: Int)]
  ]

prop_aClockIsWhatEachSideHasLeftAndWhoseIsGoingDown :: Property
prop_aClockIsWhatEachSideHasLeftAndWhoseIsGoingDown = withTests 1 . property $ do
  case clockFrom clockEvent of
    Nothing -> annotate "it read no clock at all" >> failure
    Just clock -> do
      -- Part of a second is not worth showing, and what is shown is
      -- rounded down, so a clock never claims time that is not there.
      clock.black === Time { seconds = 301, periods = 5, each = 30 }
      clock.white === Time { seconds = 60, periods = 5, each = 30 }
      -- Whose is going down comes from who the server says is to play,
      -- against the two players it names in the same breath.
      clock.running === Just Black
  -- And it arrives at the game as a clock rather than as nothing.
  incoming table "game/1234/clock" [clockEvent] === (Ticked <$> clockFrom clockEvent)

prop_theOtherSideBeingToPlayIsTheOtherSidesClock :: Property
prop_theOtherSideBeingToPlayIsTheOtherSidesClock = withTests 1 . property $ do
  let theirs = replacing "current_player" (toJSON (99 :: Int)) clockEvent
  (whoseTurn <$> clockFrom theirs) === Just (Just White)

prop_aGameNobodyIsWaitingOnHasNeitherClockGoingDown :: Property
prop_aGameNobodyIsWaitingOnHasNeitherClockGoingDown = withTests 1 . property $ do
  -- Paused, which a correspondence game is for somebody's holiday.
  let paused = replacing "paused_since" (toJSON (1700000000000 :: Int)) clockEvent
  (whoseTurn <$> clockFrom paused) === Just Nothing
  -- Or nobody to play at all, which is a game that has not started.
  let nobody = replacing "current_player" Null clockEvent
  (whoseTurn <$> clockFrom nobody) === Just Nothing

prop_oneLotOfTimeAndNothingAfterItIsJustANumber :: Property
prop_oneLotOfTimeAndNothingAfterItIsJustANumber = withTests 1 . property $ do
  -- An absolute game, where the server writes the time as a number
  -- rather than as an object, and there are no periods to come.
  timeFrom (toJSON (125 :: Int)) === Just Time { seconds = 125, periods = 0, each = 0 }
  -- A game counted some other way, where the parts this program does
  -- not show are left out rather than guessed at. What is running is
  -- still the time that is running.
  timeFrom (object ["thinking_time" .= (45 :: Int), "skip_bonus" .= False])
    === Just Time { seconds = 45, periods = 0, each = 0 }

prop_aGameWithNoClockOnItShowsNoClock :: Property
prop_aGameWithNoClockOnItShowsNoClock = withTests 1 . property $ do
  -- Not every game is timed, and one that is not says nothing about
  -- how long anybody has left. A window with no clock to show is
  -- better than one showing a clock of zero.
  clockFrom (object ["game_id" .= (1234 :: Int)]) === Nothing
  clockFrom (object []) === Nothing
  timeFrom (String "soon") === Nothing
  incoming table "game/1234/clock" [object []] === Nothing

-- | Whose clock is going down.
whoseTurn :: Clock -> Maybe Color
whoseTurn clock = clock.running

-- | The same object with one field put in or changed.
replacing :: Text -> Value -> Value -> Value
replacing key held (Object fields) =
  Object (KeyMap.insert (Key.fromText key) held fields)
replacing _ _ other = other

isLeft :: Either a b -> Bool
isLeft = either (const True) (const False)

-- * Where a game starts
--------------------------

-- | What the server says about a handicap game when it is joined.
-- Black's two stones are down and White is the one to move.
handicapGame :: Value
handicapGame = object
  [ "width" .= (9 :: Int)
  , "handicap" .= (2 :: Int)
  , "initial_player" .= ("white" :: Text)
  , "initial_state" .= object
    ["black" .= ("ccgg" :: Text), "white" .= ("" :: Text)]
  , "black_player_id" .= (77 :: Int)
  , "white_player_id" .= (88 :: Int)
  , "moves" .= ([] :: [Value])
  ]

prop_stonesAreTwoLettersEachWithNothingBetweenThem :: Property
prop_stonesAreTwoLettersEachWithNothingBetweenThem =
  withTests 1 . property $ do
    decodeStones "ccgg" === [Coord 2 2, Coord 6 6]
    decodeStones "aa" === [Coord 0 0]
    decodeStones "" === []
    -- Half a stone is no stone.
    decodeStones "ccg" === [Coord 2 2]
    decodeStones "??" === []

prop_aHandicapGameStartsWithStonesDownAndWhiteToMove :: Property
prop_aHandicapGameStartsWithStonesDownAndWhiteToMove =
  withTests 1 . property $ do
    openingFrom 77 handicapGame
      === Right Opening { width  = 9
                        , stones = [(Black, Coord 2 2), (Black, Coord 6 6)]
                        , opens  = White
                        , yours  = Black
                        }

prop_whichColourThePlayerHasComesFromTheirNumber :: Property
prop_whichColourThePlayerHasComesFromTheirNumber =
  withTests 1 . property $ do
    -- Nothing at this end decided it, because somebody else did the
    -- pairing.
    fmap (.yours) (openingFrom 77 handicapGame) === Right Black
    fmap (.yours) (openingFrom 88 handicapGame) === Right White

prop_aGameUnderWayIsPlayedOutRatherThanTakenAsRead :: Property
prop_aGameUnderWayIsPlayedOutRatherThanTakenAsRead =
  withTests 1 . property $ do
    -- What is wanted is the position the moves lead to, and stones
    -- come off along the way, so they are played rather than added up.
    let started = object
          [ "width" .= (9 :: Int)
          , "initial_player" .= ("black" :: Text)
          , "black_player_id" .= (88 :: Int)
          , "white_player_id" .= (77 :: Int)
          , "moves" .= [arriving 3 3, arriving 5 5]
          ]
    openingFrom 77 started
      === Right Opening { width  = 9
                        , stones = [(Black, Coord 3 3), (White, Coord 5 5)]
                        , opens  = Black
                        , yours  = White
                        }

prop_aGameTheseRulesRefuseIsSaidSoRatherThanHalfTaken :: Property
prop_aGameTheseRulesRefuseIsSaidSoRatherThanHalfTaken =
  withTests 1 . property $ do
    let impossible = object
          [ "width" .= (9 :: Int)
          , "initial_player" .= ("black" :: Text)
          , "black_player_id" .= (88 :: Int)
          , "white_player_id" .= (77 :: Int)
          , "moves" .= [arriving 3 3, arriving 3 3]
          ]
    case openingFrom 77 impossible of
      Left why -> assert (Text.isInfixOf "refuse" why)
      Right _  -> annotate "it took a game it cannot play" >> failure
    -- And a game missing what it takes to start is refused too.
    assert (isLeft (openingFrom 77 (object ["width" .= (9 :: Int)])))
    assert (isLeft (openingFrom 77 (object [])))

prop_howFarAlongAGameIsAndWhoOpenedIt :: Property
prop_howFarAlongAGameIsAndWhoOpenedIt = withTests 1 . property $ do
  countFrom handicapGame === Just 0
  openerFrom handicapGame === Just White
  countFrom (object ["moves" .= [arriving 3 3, arriving 5 5]]) === Just 2
  openerFrom (object ["initial_player" .= ("black" :: Text)]) === Just Black
  countFrom (object []) === Nothing
  openerFrom (object []) === Nothing

prop_theGameArrivesBeforeAnythingElseDoes :: Property
prop_theGameArrivesBeforeAnythingElseDoes = withTests 1 . property $ do
  -- The table says this player is number 77, and 77 is Black in this
  -- game.
  incoming table "game/1234/gamedata" [handicapGame]
    === Just
          (Began Opening { width  = 9
                         , stones = [(Black, Coord 2 2), (Black, Coord 6 6)]
                         , opens  = White
                         , yours  = Black
                         }
          )

tests :: Group
tests = $$(discover)
