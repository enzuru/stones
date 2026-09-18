{-# LANGUAGE OverloadedStrings #-}
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

import           Data.Maybe                     ( isJust )
import           Data.Text                      ( Text )
import           Hedgehog

import           Go.Game
import           Go.Types
import           Stones.Engine
import           Stones.Session

-- | An opponent that does nothing and says so.
--
-- None of these run. 'step' is a function, and what it hands back is
-- an action the window would run, which these tests do not.
silent :: Engine
silent = Engine { engineName    = "nobody"
                , engineNewGame = \_ -> pure (Right ())
                , engineNotify  = \_ _ -> pure (Right ())
                , engineGenMove = \_ -> pure (Right Pass)
                , engineUndo    = \_ -> pure (Right ())
                , engineScore   = pure (Right "0")
                , engineClose   = pure ()
                }

-- | A 9x9 game, played as Black, whose opponent has just started.
game :: Session
game = stepSession (opened silent (starting Black 9))

-- | Why a game stopped, if it did.
brokenBecause :: Session -> Maybe Text
brokenBecause session = case sessionOpponent session of
  Gone why -> Just why
  _        -> Nothing

-- | Where a game lands after an event.
after :: Session -> SessionEvent -> Session
after session event = stepSession (step session event)

-- | Whether an event asked the opponent anything.
asked :: Session -> SessionEvent -> Bool
asked session event = isJust (stepAsk (step session event))

prop_aStartedGameIsThePlayersToMove :: Property
prop_aStartedGameIsThePlayersToMove = withTests 1 . property $ do
  let waiting = starting Black 9
  playable waiting === False
  boardSize (sessionGame waiting) === 9
  playable game === True
  sessionNote game === Nothing

prop_theOpponentOpensWhenThePlayerHasWhite :: Property
prop_theOpponentOpensWhenThePlayerHasWhite = withTests 1 . property $ do
  let opening = opened silent (starting White 13)
  -- Black moves first, so a player with White waits, and the opening
  -- move is asked for rather than clicked.
  playable (stepSession opening) === False
  assert (isJust (stepAsk opening))

prop_aMoveGoesDownAndTheOpponentIsAsked :: Property
prop_aMoveGoesDownAndTheOpponentIsAsked = withTests 1 . property $ do
  let played = after game (Clicked (Coord 3 3))
  gameLast (sessionGame played) === Just (Coord 3 3)
  gameTurn (sessionGame played) === White
  assert (asked game (Clicked (Coord 3 3)))
  playable played === False

prop_theBoardTakesNoClicksWhileTheOpponentThinks :: Property
prop_theBoardTakesNoClicksWhileTheOpponentThinks = withTests 1 . property $ do
  let played  = after game (Clicked (Coord 3 3))
      ignored = after played (Clicked (Coord 4 4))
  gameLast (sessionGame ignored) === Just (Coord 3 3)
  gameMoves (sessionGame ignored) === gameMoves (sessionGame played)
  asked played (Clicked (Coord 4 4)) === False

prop_aGameWithNoOpponentYetTakesNoClicks :: Property
prop_aGameWithNoOpponentYetTakesNoClicks = withTests 1 . property $ do
  let waiting = starting Black 9
      ignored = after waiting (Clicked (Coord 3 3))
  gameMoves (sessionGame ignored) === []
  asked waiting (Clicked (Coord 3 3)) === False

prop_anAnswerNobodyAskedForIsDropped :: Property
prop_anAnswerNobodyAskedForIsDropped = withTests 1 . property $ do
  -- The game is idle, so this belongs to a question that has already
  -- been answered, and playing it would put a stone down out of turn.
  let stray = after game (Answered (Moved (Play (Coord 5 5))))
  gameMoves (sessionGame stray) === []

prop_aBrokenOpponentStopsTheGame :: Property
prop_aBrokenOpponentStopsTheGame = withTests 1 . property $ do
  let played = after game (Clicked (Coord 3 3))
      broken = after played (Answered (Failed "the pipe closed"))
      later  = after broken (Clicked (Coord 5 5))
  brokenBecause broken === Just "the pipe closed"
  playable broken === False
  canUndo broken === False
  gameLast (sessionGame later) === Just (Coord 3 3)

prop_theOpponentResigningEndsTheGame :: Property
prop_theOpponentResigningEndsTheGame = withTests 1 . property $ do
  let played   = after game (Clicked (Coord 3 3))
      resigned = after played (Answered (Moved Resign))
  finished (sessionGame resigned) === True
  winnerByResignation (sessionGame resigned) === Just Black
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
  sessionNote onTop === Just (Refused Occupied)
  gameMoves (sessionGame onTop) === gameMoves (sessionGame back)
  -- A move that goes down is not the moment to still be complaining
  -- about the one before it.
  sessionNote moved === Nothing

prop_undoTakesBackBothMoves :: Property
prop_undoTakesBackBothMoves = withTests 1 . property $ do
  let played   = after game (Clicked (Coord 3 3))
      answered = after played (Answered (Moved (Play (Coord 4 4))))
      undone   = after answered UndoPressed
  gameMoves (sessionGame answered) === [Play (Coord 4 4), Play (Coord 3 3)]
  canUndo answered === True
  gameMoves (sessionGame undone) === []
  gameTurn (sessionGame undone) === Black
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
  finished (sessionGame both) === True
  assert (asked passed (Answered (Moved Pass)))
  sessionNote scored === Just (Result "B+2.5")

prop_resigningEndsTheGameWithoutAskingAnything :: Property
prop_resigningEndsTheGameWithoutAskingAnything = withTests 1 . property $ do
  let resigned = after game ResignPressed
  finished (sessionGame resigned) === True
  winnerByResignation (sessionGame resigned) === Just White
  asked game ResignPressed === False

tests :: Group
tests = $$(discover)
