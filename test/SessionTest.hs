-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | What each thing the player does turns one game into.
--
-- The opponent here is a fake. 'Stones.Engine.Engine' is a record of
-- actions, so an opponent that answers whatever the test wants is a
-- record with those answers in it, and a game can be driven without a
-- process or a window.
module SessionTest
  ( tests
  )
where

import           Data.Text                      ( Text )
import           GI.Gtk.Declarative.App.Simple   ( Cmd
                                                , Transition(..)
                                                , jobsOf
                                                , none
                                                )
import           Hedgehog
import qualified Pipes.Prelude                 as Pipes

import           Go.Game
import           Go.Types
import           Stones.Engine
import           Stones.Session

-- | An opponent that does nothing and says so.
--
-- None of these run. 'step' is a function, and what it hands back is
-- an action the window would run, which these tests do not.
silent :: Engine
silent = Engine { name    = "nobody"
                , newGame = \_ -> pure (Right ())
                , notify  = \_ _ -> pure (Right ())
                , genMove = \_ -> pure (Right Pass)
                , undo    = \_ -> pure (Right ())
                , score   = pure (Right "0")
                , close   = pure ()
                }

-- | A 9x9 game, played as Black, whose opponent has just started.
game :: Session
game = landed (opened silent (starting Black 9))

-- | The same, against an opponent that answers differently.
gameWith :: Engine -> Session
gameWith engine = landed (opened engine (starting Black 9))

-- | Run what a game asks of its opponent, and answer with what it
-- said. These are the only tests that run one: the rest are about
-- where the game lands, which is decided before anything is asked.
--
-- The job has to run under no name. A named job stops whatever is
-- running under that name, and nothing a game asks for can be stopped
-- part way through.
ran :: Played -> PropertyT IO Reply
ran given = case jobsOf (commandOf given) of
  [(Nothing, job)] -> evalIO (Pipes.toListM job) >>= \case
    [Answered reply] -> pure reply
    answers          -> annotateShow answers >> failure
  []    -> annotate "nothing was asked" >> failure
  other -> annotateShow (map fst other) >> failure

-- | Why a game stopped, if it did.
brokenBecause :: Session -> Maybe Text
brokenBecause session = case session.opponent of
  Gone why -> Just why
  _        -> Nothing

-- | Whether a transition asked for anything.
asked' :: Played -> Bool
asked' = not . null . jobsOf . commandOf

-- | The command a transition answers with.
commandOf :: Played -> Cmd SessionEvent
commandOf (Transition _ cmd) = cmd
commandOf Exit               = none

-- | The game a transition leaves behind. Nothing a game does ends the
-- window, so the other case cannot happen.
landed :: Played -> Session
landed (Transition session _) = session
landed Exit                   = error "a game ended the window"

-- | Where a game lands after an event.
after :: Session -> SessionEvent -> Session
after session event = landed (step session event)

-- | Whether an event asked the opponent anything.
asked :: Session -> SessionEvent -> Bool
asked session event = not (null (jobsOf (commandOf (step session event))))

prop_aStartedGameIsThePlayersToMove :: Property
prop_aStartedGameIsThePlayersToMove = withTests 1 . property $ do
  let waiting = starting Black 9
  playable waiting === False
  boardSize waiting.game === 9
  playable game === True
  game.note === Nothing

prop_theOpponentOpensWhenThePlayerHasWhite :: Property
prop_theOpponentOpensWhenThePlayerHasWhite = withTests 1 . property $ do
  let opening = opened silent (starting White 13)
  -- Black moves first, so a player with White waits, and the opening
  -- move is asked for rather than clicked.
  playable (landed opening) === False
  assert (asked' opening)

prop_aMoveGoesDownAndTheOpponentIsAsked :: Property
prop_aMoveGoesDownAndTheOpponentIsAsked = withTests 1 . property $ do
  let played = after game (Clicked (Coord 3 3))
  played.game.last === Just (Coord 3 3)
  played.game.turn === White
  assert (asked game (Clicked (Coord 3 3)))
  playable played === False

prop_theBoardTakesNoClicksWhileTheOpponentThinks :: Property
prop_theBoardTakesNoClicksWhileTheOpponentThinks = withTests 1 . property $ do
  let played  = after game (Clicked (Coord 3 3))
      ignored = after played (Clicked (Coord 4 4))
  ignored.game.last === Just (Coord 3 3)
  ignored.game.moves === played.game.moves
  asked played (Clicked (Coord 4 4)) === False

prop_aGameWithNoOpponentYetTakesNoClicks :: Property
prop_aGameWithNoOpponentYetTakesNoClicks = withTests 1 . property $ do
  let waiting = starting Black 9
      ignored = after waiting (Clicked (Coord 3 3))
  ignored.game.moves === []
  asked waiting (Clicked (Coord 3 3)) === False

prop_anAnswerNobodyAskedForIsDropped :: Property
prop_anAnswerNobodyAskedForIsDropped = withTests 1 . property $ do
  -- The game is idle, so this belongs to a question that has already
  -- been answered, and playing it would put a stone down out of turn.
  let stray = after game (Answered (Moved (Play (Coord 5 5))))
  stray.game.moves === []

prop_aBrokenOpponentStopsTheGame :: Property
prop_aBrokenOpponentStopsTheGame = withTests 1 . property $ do
  let played = after game (Clicked (Coord 3 3))
      broken = after played (Answered (Failed "the pipe closed"))
      later  = after broken (Clicked (Coord 5 5))
  brokenBecause broken === Just "the pipe closed"
  playable broken === False
  canUndo broken === False
  later.game.last === Just (Coord 3 3)

prop_theOpponentResigningEndsTheGame :: Property
prop_theOpponentResigningEndsTheGame = withTests 1 . property $ do
  let played   = after game (Clicked (Coord 3 3))
      resigned = after played (Answered (Moved Resign))
  finished resigned.game === True
  winnerByResignation resigned.game === Just Black
  playable resigned === False

prop_aMoveTheRulesRefuseStopsTheGame :: Property
prop_aMoveTheRulesRefuseStopsTheGame = withTests 1 . property $ do
  -- The opponent answering with a point that already has a stone on it
  -- means the two boards have drifted apart.
  let played   = after game (Clicked (Coord 3 3))
      disagree = after played (Answered (Moved (Play (Coord 3 3))))
  playable disagree === False
  canUndo disagree === False
  assert (brokenBecause disagree /= Nothing)

prop_aClickTheRulesRefuseIsSaidSoAndThenForgotten :: Property
prop_aClickTheRulesRefuseIsSaidSoAndThenForgotten = withTests 1 . property $ do
  let played  = after game (Clicked (Coord 3 3))
      back    = after played (Answered (Moved (Play (Coord 4 4))))
      onTop   = after back (Clicked (Coord 3 3))
      moved   = after onTop (Clicked (Coord 5 5))
  -- The point already has a stone on it, so nothing is played and the
  -- game says why.
  onTop.note === Just (Refused Occupied)
  onTop.game.moves === back.game.moves
  -- A move that goes down is not the moment to still be complaining
  -- about the one before it.
  moved.note === Nothing

prop_undoTakesBackBothMoves :: Property
prop_undoTakesBackBothMoves = withTests 1 . property $ do
  let played   = after game (Clicked (Coord 3 3))
      answered = after played (Answered (Moved (Play (Coord 4 4))))
      undone   = after answered UndoPressed
  answered.game.moves === [Play (Coord 4 4), Play (Coord 3 3)]
  canUndo answered === True
  undone.game.moves === []
  undone.game.turn === Black
  assert (asked answered UndoPressed)

prop_thereIsNothingToTakeBackAtTheStart :: Property
prop_thereIsNothingToTakeBackAtTheStart = withTests 1 . property $ do
  canUndo game === False
  asked game UndoPressed === False

prop_twoPassesEndTheGameAndAskForAScore :: Property
prop_twoPassesEndTheGameAndAskForAScore = withTests 1 . property $ do
  let passed = after game Passed
      both   = after passed (Answered (Moved Pass))
      scored = after both (Answered (Scored "B+2.5"))
  finished both.game === True
  assert (asked passed (Answered (Moved Pass)))
  scored.note === Just (Result "B+2.5")

prop_resigningEndsTheGameWithoutAskingAnything :: Property
prop_resigningEndsTheGameWithoutAskingAnything = withTests 1 . property $ do
  let resigned = after game ResignPressed
  finished resigned.game === True
  winnerByResignation resigned.game === Just White
  asked game ResignPressed === False

-- * What the opponent is asked, and what it answers
------------------------------------------------------

prop_aMoveIsToldToTheOpponentAndAnAnswerAsked :: Property
prop_aMoveIsToldToTheOpponentAndAnAnswerAsked = withTests 1 . property $ do
  reply <- ran (step game (Clicked (Coord 3 3)))
  -- The silent opponent always passes, and a pass is what comes back.
  case reply of
    Moved Pass -> success
    other      -> annotateShow other >> failure

prop_anOpponentThatWillNotBeToldIsAFailure :: Property
prop_anOpponentThatWillNotBeToldIsAFailure = withTests 1 . property $ do
  let deaf = silent { notify = \_ _ -> pure (Left "it would not listen") }
  reply <- ran (step (gameWith deaf) (Clicked (Coord 3 3)))
  case reply of
    Failed why -> why === "it would not listen"
    other      -> annotateShow other >> failure

prop_anOpponentThatWillNotMoveIsAFailure :: Property
prop_anOpponentThatWillNotMoveIsAFailure = withTests 1 . property $ do
  let stuck = silent { genMove = \_ -> pure (Left "it would not move") }
  reply <- ran (step (gameWith stuck) (Clicked (Coord 3 3)))
  case reply of
    Failed why -> why === "it would not move"
    other      -> annotateShow other >> failure

prop_theOpeningMoveIsAskedForWhenThePlayerHasWhite :: Property
prop_theOpeningMoveIsAskedForWhenThePlayerHasWhite =
  withTests 1 . property $ do
    reply <- ran (opened silent (starting White 9))
    case reply of
      Moved Pass -> success
      other      -> annotateShow other >> failure

prop_theSecondPassAsksForTheScore :: Property
prop_theSecondPassAsksForTheScore = withTests 1 . property $ do
  let passed = after game Passed
      both   = after passed (Answered (Moved Pass))
  -- Black passed, White passed, so the game has ended and the next
  -- thing asked is what it came to.
  finished both.game === True
  reply <- ran (step passed (Answered (Moved Pass)))
  case reply of
    Scored out -> out === "0"
    other      -> annotateShow other >> failure

prop_anOpponentThatWillNotCountIsAFailure :: Property
prop_anOpponentThatWillNotCountIsAFailure = withTests 1 . property $ do
  let vague  = silent { score = pure (Left "it would not count") }
      passed = after (gameWith vague) Passed
  reply <- ran (step passed (Answered (Moved Pass)))
  case reply of
    Failed why -> why === "it would not count"
    other      -> annotateShow other >> failure

prop_takingBackToYourOwnTurnAsksForNothingMore :: Property
prop_takingBackToYourOwnTurnAsksForNothingMore = withTests 1 . property $ do
  let played   = after game (Clicked (Coord 3 3))
      answered = after played (Answered (Moved (Play (Coord 4 4))))
  reply <- ran (step answered UndoPressed)
  -- Two moves came off, so it is the player's turn and there is
  -- nothing to ask the opponent for.
  case reply of
    Ready -> success
    other -> annotateShow other >> failure

prop_takingBackToTheOpponentsTurnAsksItToMove :: Property
prop_takingBackToTheOpponentsTurnAsksItToMove = withTests 1 . property $ do
  -- The player has White, so the opponent opened. There is one move to
  -- take back, and taking it back leaves the opponent to play again.
  let opening  = landed (opened silent (starting White 9))
      answered = after opening (Answered (Moved (Play (Coord 3 3))))
  canUndo answered === True
  reply <- ran (step answered UndoPressed)
  case reply of
    Moved Pass -> success
    other      -> annotateShow other >> failure

prop_anOpponentThatWillNotGoBackIsAFailure :: Property
prop_anOpponentThatWillNotGoBackIsAFailure = withTests 1 . property $ do
  let stubborn = silent { undo = \_ -> pure (Left "it would not go back") }
      played   = after (gameWith stubborn) (Clicked (Coord 3 3))
      answered = after played (Answered (Moved (Play (Coord 4 4))))
  reply <- ran (step answered UndoPressed)
  case reply of
    Failed why -> why === "it would not go back"
    other      -> annotateShow other >> failure

prop_aGameThatCouldNotStartSaysSo :: Property
prop_aGameThatCouldNotStartSaysSo = withTests 1 . property $ do
  let dead = couldNotOpen "no such program" (starting Black 9)
  brokenBecause dead === Just "no such program"
  playable dead === False
  canUndo dead === False
  -- Nothing is asked of an opponent that is not there.
  asked dead (Clicked (Coord 3 3)) === False
  asked dead UndoPressed === False
  asked dead ResignPressed === False

prop_whatAGameIsPlayedOn :: Property
prop_whatAGameIsPlayedOn = withTests 1 . property $ do
  sessionSize game === 9
  sessionSize (starting Black 19) === 19
  opponentOf game === "nobody"
  opponentOf (starting Black 9) === "starting"
  opponentOf (couldNotOpen "gone" (starting Black 9)) === "no engine"
  opponentOf (after game (Clicked (Coord 3 3))) === "nobody"

prop_passingSecondTellsTheOpponentAndAsksForTheScore :: Property
prop_passingSecondTellsTheOpponentAndAsksForTheScore =
  withTests 1 . property $ do
    -- The opponent passed first, so the player's pass is the one that
    -- ends the game. It still has to be told about that pass before it
    -- can be asked what the game came to.
    let played   = after game (Clicked (Coord 3 3))
        theyPass = after played (Answered (Moved Pass))
        over     = after theyPass Passed
    finished over.game === True
    reply <- ran (step theyPass Passed)
    case reply of
      Scored out -> out === "0"
      other      -> annotateShow other >> failure

prop_anOpponentThatWillNotBeToldOfTheLastPassIsAFailure :: Property
prop_anOpponentThatWillNotBeToldOfTheLastPassIsAFailure =
  withTests 1 . property $ do
    let deaf     = silent { notify = \_ _ -> pure (Left "it stopped listening") }
        played   = after (gameWith deaf) (Clicked (Coord 3 3))
        theyPass = after played (Answered (Moved Pass))
    reply <- ran (step theyPass Passed)
    case reply of
      Failed why -> why === "it stopped listening"
      other      -> annotateShow other >> failure

prop_aDisagreementSaysWhichWayItWent :: Property
prop_aDisagreementSaysWhichWayItWent = withTests 1 . property $ do
  let played   = after game (Clicked (Coord 3 3))
      disagree = after played (Answered (Moved (Play (Coord 3 3))))
  brokenBecause disagree
    === Just ("The engine and the board disagree: " <> describeIllegal Occupied)

prop_resigningWhenItIsNotYourTurnDoesNothing :: Property
prop_resigningWhenItIsNotYourTurnDoesNothing = withTests 1 . property $ do
  let played   = after game (Clicked (Coord 3 3))
      resigned = after played ResignPressed
  -- The opponent is thinking, so there is nothing to resign from yet.
  finished resigned.game === False
  asked played ResignPressed === False
  -- And a game that has already ended cannot be resigned either.
  let over  = after (after game Passed) (Answered (Moved Pass))
      again = after over ResignPressed
  again.game.moves === over.game.moves

tests :: Group
tests = $$(discover)