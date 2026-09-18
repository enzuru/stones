{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | What each event does to the window's state.
--
-- The engine here is a fake. 'Stones.Engine.Engine' is a record of
-- actions, so an opponent that answers whatever the test wants is a
-- record with those answers in it, and the state machine can be driven
-- without a process or a window.
module AppTest
  ( tests
  )
where

import           Hedgehog

import           GI.Gtk.Declarative.App.Simple  ( Transition(..) )

import           Go.Game
import           Go.Types
import           Stones.App
import           Stones.Engine

-- | An opponent that does nothing and says so.
--
-- None of these run: 'update'' is a pure function, and what it hands
-- back is a command the application would run, which the test does not.
silent :: Engine
silent = Engine { engineName    = "nobody"
                , engineNewGame = \_ -> pure (Right ())
                , engineNotify  = \_ _ -> pure (Right ())
                , engineGenMove = \_ -> pure (Right Pass)
                , engineUndo    = \_ -> pure (Right ())
                , engineScore   = pure (Right "0")
                , engineClose   = pure ()
                }

-- | The state the window opens with, playing Black on a 9x9 board.
opening :: State
opening = startingState silent Black 9

-- | The state after the event that sets the first board up.
running :: State
running = next opening (NewGamePressed 9)

-- | The state an event leads to. A transition that exits has no state,
-- and nothing here exits.
next :: State -> Event -> State
next state event = case update' state event of
  Transition state' _ -> state'
  Exit                -> error "the transition exited"

prop_theFirstBoardIsSetUpRatherThanQueued :: Property
prop_theFirstBoardIsSetUpRatherThanQueued = withTests 1 . property $ do
  -- The window opens with nothing asked of the engine, so the event it
  -- sends itself has to start the game rather than wait for an answer
  -- that is not coming.
  stateThinking opening === False
  statePending opening === Nothing
  stateThinking running === True
  statePending running === Nothing

prop_aMoveGoesDownAndTheEngineIsAsked :: Property
prop_aMoveGoesDownAndTheEngineIsAsked = withTests 1 . property $ do
  let ready  = next running (FromEngine Ready)
      played = next ready (Clicked (Coord 3 3))
  stateThinking ready === False
  gameLast (stateGame played) === Just (Coord 3 3)
  gameTurn (stateGame played) === White
  stateThinking played === True

prop_theBoardTakesNoClicksWhileTheEngineThinks :: Property
prop_theBoardTakesNoClicksWhileTheEngineThinks = withTests 1 . property $ do
  let ready   = next running (FromEngine Ready)
      played  = next ready (Clicked (Coord 3 3))
      ignored = next played (Clicked (Coord 4 4))
  gameLast (stateGame ignored) === Just (Coord 3 3)
  gameMoves (stateGame ignored) === gameMoves (stateGame played)

prop_aNewGameAskedForWhileThinkingWaitsAndThenStarts :: Property
prop_aNewGameAskedForWhileThinkingWaitsAndThenStarts =
  withTests 1 . property $ do
    let ready   = next running (FromEngine Ready)
        played  = next ready (Clicked (Coord 3 3))
        asked   = next played (NewGamePressed 13)
        started = next asked (FromEngine (Moved (Play (Coord 4 4))))
    -- Nothing is asked of the engine while it is answering.
    statePending asked === Just 13
    boardSize (stateGame asked) === 9
    -- Its answer lets the new board through, and the answer itself is
    -- dropped: it belongs to the game that has just been replaced.
    statePending started === Nothing
    boardSize (stateGame started) === 13
    gameMoves (stateGame started) === []
    stateThinking started === True

prop_aBrokenEngineStopsTheGame :: Property
prop_aBrokenEngineStopsTheGame = withTests 1 . property $ do
  let ready  = next running (FromEngine Ready)
      played = next ready (Clicked (Coord 3 3))
      broken = next played (FromEngine (Failed "the pipe closed"))
      after  = next broken (Clicked (Coord 5 5))
  stateBroken broken === True
  stateThinking broken === False
  stateMessage broken === "the pipe closed"
  -- A broken game takes no more moves.
  gameLast (stateGame after) === Just (Coord 3 3)

prop_theEngineResigningEndsTheGame :: Property
prop_theEngineResigningEndsTheGame = withTests 1 . property $ do
  let ready    = next running (FromEngine Ready)
      played   = next ready (Clicked (Coord 3 3))
      resigned = next played (FromEngine (Moved Resign))
  finished (stateGame resigned) === True
  winnerByResignation (stateGame resigned) === Just Black
  stateThinking resigned === False

prop_aMoveTheRulesRefuseStopsTheGame :: Property
prop_aMoveTheRulesRefuseStopsTheGame = withTests 1 . property $ do
  -- The engine answering with a point that already has a stone on it
  -- means the two boards have drifted apart.
  let ready    = next running (FromEngine Ready)
      played   = next ready (Clicked (Coord 3 3))
      disagree = next played (FromEngine (Moved (Play (Coord 3 3))))
  stateBroken disagree === True
  stateThinking disagree === False

prop_undoTakesBackBothMoves :: Property
prop_undoTakesBackBothMoves = withTests 1 . property $ do
  let ready    = next running (FromEngine Ready)
      played   = next ready (Clicked (Coord 3 3))
      answered = next played (FromEngine (Moved (Play (Coord 4 4))))
      undone   = next answered UndoPressed
  gameMoves (stateGame answered) === [Play (Coord 4 4), Play (Coord 3 3)]
  gameMoves (stateGame undone) === []
  gameTurn (stateGame undone) === Black
  stateThinking undone === True

prop_twoPassesEndTheGameAndAskForAScore :: Property
prop_twoPassesEndTheGameAndAskForAScore = withTests 1 . property $ do
  let ready  = next running (FromEngine Ready)
      passed = next ready Passed
      both   = next passed (FromEngine (Moved Pass))
      scored = next both (FromEngine (Scored "B+2.5"))
  finished (stateGame both) === True
  stateThinking both === True
  stateMessage scored === "Result: B+2.5"
  stateThinking scored === False

tests :: Group
tests = $$(discover)
