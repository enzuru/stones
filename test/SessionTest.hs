-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

-- | What each thing that happens turns one game into.
--
-- The opponent here is a fake. 'Stones.Opponent.Opponent' is a record
-- of actions, so one that takes whatever a test says and writes it
-- down is a record with those actions in it, and a game can be driven
-- without a process or a window.
--
-- Two kinds of test, because the seam has two halves. What a game
-- says to its opponent is read out of the fake's book. What arrives
-- from the other side is an ordinary event, so those tests run no
-- jobs at all.
module SessionTest
  ( tests
  )
where

import           Data.IORef
import           Data.Text                      ( Text )
import           GI.Gtk.Declarative.App.Simple  ( Cmd
                                                , Transition(..)
                                                , jobsOf
                                                , none
                                                )
import           Hedgehog
import qualified Pipes.Prelude                 as Pipes

import           Go.Board                       ( stoneAt )
import           Go.Game
import           Go.Types
import           Stones.Opponent
import           Stones.Session

-- | An opponent that takes everything and says nothing.
silent :: Opponent
silent = Opponent { name  = "nobody"
                  , tell  = \_ -> pure (Right ())
                  , hear  = pure ()
                  , leave = pure ()
                  }

-- | One that will not be told anything, and says why.
deaf :: Text -> Opponent
deaf why = silent { tell = \_ -> pure (Left why) }

-- | One that writes down what it is told, and a way to read the book.
recording :: IO (Opponent, IO [Told])
recording = do
  book <- newIORef []
  pure (silent { tell = \this -> Right () <$ modifyIORef' book (<> [this]) }
       , readIORef book
       )

-- | A 9x9 game as this colour against this opponent, from the moment
-- it opens.
--
-- Opening is itself a step, and it says something when the player has
-- White, so the tests below start from the step rather than from the
-- game it leaves behind.
opening :: Color -> Opponent -> Step
opening side them = opened them (starting side 9)

-- | The game an opened one leaves behind, for the tests that say
-- nothing.
gameWith :: Opponent -> Session
gameWith them = landed (opening Black them)

-- | The same, against the silent one.
game :: Session
game = gameWith silent

-- | What a game says to its opponent when these things happen to it.
--
-- The game is built around the fake, because the book belongs to the
-- opponent the game is holding.
saying :: (Opponent -> Step) -> [SessionEvent] -> PropertyT IO [Told]
saying begin events = do
  (them, book) <- evalIO recording
  let go given []              = () <$ ran given
      go given (this : after') = do
        _ <- ran given
        go (step (landed given) this) after'
  go (begin them) events
  evalIO book

-- | Run whatever a step asked for, and answer with what came back.
ran :: Step -> PropertyT IO [SessionEvent]
ran given =
  concat <$> traverse (evalIO . Pipes.toListM . snd) (jobsOf (commandOf given))

-- | Why a game stopped, if it did.
brokenBecause :: Session -> Maybe Text
brokenBecause session = case session.standing of
  Gone why -> Just why
  _        -> Nothing

-- | Whether a transition asked for anything.
asked' :: Step -> Bool
asked' = not . null . jobsOf . commandOf

-- | The command a transition answers with.
commandOf :: Step -> Cmd SessionEvent
commandOf (Transition _ cmd) = cmd
commandOf Exit               = none

-- | The game a transition leaves behind. Nothing a game does ends the
-- window, so the other case cannot happen.
landed :: Step -> Session
landed (Transition session _) = session
landed Exit                   = error "a game ended the window"

-- | Where a game lands after an event.
after :: Session -> SessionEvent -> Session
after session event = landed (step session event)

-- | Whether an event said anything to the opponent.
asked :: Session -> SessionEvent -> Bool
asked session event = asked' (step session event)

-- * Where a game stands
------------------------

prop_aStartedGameIsThePlayersToMove :: Property
prop_aStartedGameIsThePlayersToMove = withTests 1 . property $ do
  let waiting = starting Black 9
  playable waiting === False
  boardSize waiting.game === 9
  playable game === True
  game.note === Nothing
  opponentOf waiting === "starting"
  opponentOf game === "nobody"

prop_aGameListensOnlyWhileItHasSomebodyToListenTo :: Property
prop_aGameListensOnlyWhileItHasSomebodyToListenTo =
  withTests 1 . property $ do
    -- The window keeps a stream open for exactly these.
    named (listening (starting Black 9)) === Nothing
    named (listening game) === Just "nobody"
    named (listening (couldNotOpen "gone" (starting Black 9))) === Nothing
  where named = fmap (.name)

-- * What a game says
---------------------

prop_aMoveIsSaidAndTheTurnIsHandedOver :: Property
prop_aMoveIsSaidAndTheTurnIsHandedOver = withTests 1 . property $ do
  said <- saying (opening Black) [Clicked (Coord 3 3)]
  said === [Moved (Play (Coord 3 3)), Asked]

prop_theOpponentIsAskedToOpenWhenThePlayerHasWhite :: Property
prop_theOpponentIsAskedToOpenWhenThePlayerHasWhite =
  withTests 1 . property $ do
    said <- saying (\them -> opened them (starting White 13)) []
    -- Opening the game is itself a step, and this is the one thing it
    -- says.
    said === [Asked]
    playable (gameWith silent) === True

prop_passingLastAsksForTheCount :: Property
prop_passingLastAsksForTheCount = withTests 1 . property $ do
  -- The player moves, the other side passes, and the player's pass is
  -- the one that ends the game. What follows it is a question rather
  -- than a turn.
  said <- saying (opening Black)
                 [Clicked (Coord 3 3), Arrived (Played White Pass), Passed]
  said === [Moved (Play (Coord 3 3)), Asked, Moved Pass, Counting]

prop_resigningSaysSoAndEndsTheGame :: Property
prop_resigningSaysSoAndEndsTheGame = withTests 1 . property $ do
  said <- saying (opening Black) [ResignPressed]
  said === [Resigned]
  let over = after game ResignPressed
  finished over.game === True
  winnerByResignation over.game === Just White

prop_undoTakesBothMovesOffAndSaysSo :: Property
prop_undoTakesBothMovesOffAndSaysSo = withTests 1 . property $ do
  said <- saying
    (opening Black)
    [Clicked (Coord 3 3), Arrived (Played White (Play (Coord 4 4))), UndoPressed]
  -- Two off, and nobody is asked to move, because the board comes back
  -- to the player.
  said === [Moved (Play (Coord 3 3)), Asked, TookBack 2]

prop_undoOnTheOtherSidesTurnAsksItToMoveAgain :: Property
prop_undoOnTheOtherSidesTurnAsksItToMoveAgain = withTests 1 . property $ do
  -- The player has White, so the other side opened. One move comes
  -- off, and it is their turn again.
  said <- saying (opening White)
                 [Arrived (Played Black (Play (Coord 3 3))), UndoPressed]
  said === [Asked, TookBack 1, Asked]

prop_chatIsSaidAndNotWrittenDownUntilItComesBack :: Property
prop_chatIsSaidAndNotWrittenDownUntilItComesBack =
  withTests 1 . property $ do
    said <- saying (opening Black) [Says "hello"]
    said === [Said "hello"]
    -- What was said shows up when it arrives, so this program shows
    -- the same words as everybody else and in the same order.
    (after game (Says "hello")).said === []
    (after game (Arrived (Spoke "me" "hello"))).said === [Chat "me" "hello"]

prop_nothingIsSaidWhenItIsNotThePlayersTurn :: Property
prop_nothingIsSaidWhenItIsNotThePlayersTurn = withTests 1 . property $ do
  let played = after game (Clicked (Coord 3 3))
  asked played (Clicked (Coord 4 4)) === False
  asked played ResignPressed === False
  asked (starting Black 9) (Clicked (Coord 3 3)) === False
  asked game UndoPressed === False

prop_aTellThatWillNotBeHeardLosesTheOpponent :: Property
prop_aTellThatWillNotBeHeardLosesTheOpponent = withTests 1 . property $ do
  answers <- ran (step (gameWith (deaf "the pipe closed")) (Clicked (Coord 3 3)))
  case answers of
    [Arrived (Lost why)] -> why === "the pipe closed"
    other                -> annotateShow other >> failure

-- * What arrives
-----------------

prop_aMoveArrivesAndGoesDown :: Property
prop_aMoveArrivesAndGoesDown = withTests 1 . property $ do
  let played  = after game (Clicked (Coord 3 3))
      theirs  = after played (Arrived (Played White (Play (Coord 4 4))))
  theirs.game.last === Just (Coord 4 4)
  playable theirs === True

prop_ourOwnMoveComingBackIsDropped :: Property
prop_ourOwnMoveComingBackIsDropped = withTests 1 . property $ do
  -- A server says every move, this program's own included. The move is
  -- already down, so the colour that arrives is no longer the colour
  -- to play, and that is what tells the echo from the news.
  let played = after game (Clicked (Coord 3 3))
      echoed = after played (Arrived (Played Black (Play (Coord 3 3))))
  echoed.game.moves === played.game.moves
  echoed.game.turn === White

prop_aMoveTheRulesRefuseStopsTheGame :: Property
prop_aMoveTheRulesRefuseStopsTheGame = withTests 1 . property $ do
  let played   = after game (Clicked (Coord 3 3))
      disagree = after played (Arrived (Played White (Play (Coord 3 3))))
  brokenBecause disagree
    === Just
          ("The other side and the board disagree: "
          <> describeIllegal Occupied
          )
  playable disagree === False
  canUndo disagree === False

prop_theOtherSideResigningEndsTheGame :: Property
prop_theOtherSideResigningEndsTheGame = withTests 1 . property $ do
  let played   = after game (Clicked (Coord 3 3))
      resigned = after played (Arrived (Played White Resign))
  finished resigned.game === True
  winnerByResignation resigned.game === Just Black
  playable resigned === False

prop_aCountArrivesAsTheResult :: Property
prop_aCountArrivesAsTheResult = withTests 1 . property $ do
  (after game (Arrived (Counted "B+2.5"))).note === Just (Result "B+2.5")

prop_movesComingOffOverThereComeOffHere :: Property
prop_movesComingOffOverThereComeOffHere = withTests 1 . property $ do
  let played = after game (Clicked (Coord 3 3))
      theirs = after played (Arrived (Played White (Play (Coord 4 4))))
      undone = after theirs (Arrived (Undid 2))
  undone.game.moves === []
  undone.game.turn === Black
  -- Nothing to take back is nothing done.
  (after game (Arrived (Undid 5))).game.moves === []
  (after game (Arrived (Undid 0))).game.moves === []

prop_aLostOpponentStopsTheGame :: Property
prop_aLostOpponentStopsTheGame = withTests 1 . property $ do
  let broken = after game (Arrived (Lost "the pipe closed"))
      later  = after broken (Clicked (Coord 5 5))
  brokenBecause broken === Just "the pipe closed"
  playable broken === False
  canUndo broken === False
  named (listening broken) === Nothing
  later.game.moves === []
  where named = fmap (.name)

prop_aClickTheRulesRefuseIsSaidSoAndThenForgotten :: Property
prop_aClickTheRulesRefuseIsSaidSoAndThenForgotten =
  withTests 1 . property $ do
    let played = after game (Clicked (Coord 3 3))
        back   = after played (Arrived (Played White (Play (Coord 4 4))))
        onTop  = after back (Clicked (Coord 3 3))
        moved  = after onTop (Clicked (Coord 5 5))
    onTop.note === Just (Refused Occupied)
    onTop.game.moves === back.game.moves
    -- A move that goes down is not the moment to still be complaining
    -- about the one before it.
    moved.note === Nothing

prop_anArrivalAfterTheEndChangesNothing :: Property
prop_anArrivalAfterTheEndChangesNothing = withTests 1 . property $ do
  let over   = after (after game Passed) (Arrived (Played White Pass))
      stray  = after over (Arrived (Played Black (Play (Coord 5 5))))
  finished over.game === True
  stray.game.moves === over.game.moves

prop_aGameThatCouldNotStartSaysSo :: Property
prop_aGameThatCouldNotStartSaysSo = withTests 1 . property $ do
  let dead = couldNotOpen "no such program" (starting Black 9)
  brokenBecause dead === Just "no such program"
  playable dead === False
  canUndo dead === False
  asked dead (Clicked (Coord 3 3)) === False
  asked dead UndoPressed === False
  asked dead ResignPressed === False
  asked dead (Says "hello") === False

prop_whatAGameIsPlayedOn :: Property
prop_whatAGameIsPlayedOn = withTests 1 . property $ do
  sessionSize game === 9
  sessionSize (starting Black 19) === 19

prop_whereTheOtherSideSaysTheGameIsIsWhereItIs :: Property
prop_whereTheOtherSideSaysTheGameIsIsWhereItIs = withTests 1 . property $ do
  -- A handicap game, which is stones already down and White to play.
  -- It also says which colour the player has, because in a game
  -- somebody else paired there was nothing here to decide it.
  let begun = after
        game
        (Arrived
          (Began Opening { width  = 9
                         , stones = [(Black, Coord 2 2), (Black, Coord 6 6)]
                         , opens  = White
                         , yours  = White
                         }
          )
        )
  begun.game.turn === White
  begun.human === White
  stoneAt begun.game.board (Coord 2 2) === Just Black
  playable begun === True

prop_aGameAlreadyUnderWayIsTakenAsItIs :: Property
prop_aGameAlreadyUnderWayIsTakenAsItIs = withTests 1 . property $ do
  -- The same thing is how a game that was left and come back to picks
  -- up where it was, so a game with moves on it takes it too.
  let played = after game (Clicked (Coord 3 3))
      begun  = after
        played
        (Arrived
          (Began Opening { width  = 9
                         , stones = [(White, Coord 5 5)]
                         , opens  = Black
                         , yours  = Black
                         }
          )
        )
  stoneAt begun.game.board (Coord 5 5) === Just White
  stoneAt begun.game.board (Coord 3 3) === Nothing
  begun.game.turn === Black

tests :: Group
tests = $$(discover)