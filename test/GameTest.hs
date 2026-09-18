-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | The rules that need more than one position: ko, passing, and
-- taking a move back.
module GameTest
  ( tests
  )
where

import           Data.List                      ( isInfixOf )
import           Hedgehog

import           Go.Board
import           Go.Game
import           Go.Types

-- | Stop the test and say why, which is what a move the rules refuse
-- calls for: there is nothing to go on with.
giveUp :: MonadTest m => String -> m a
giveUp why = annotate why >> failure

-- | Play these moves in turn, starting with Black, and fail at the
-- first one the rules refuse.
run :: MonadTest m => [(Int, Int)] -> Game -> m Game
run [] game = pure game
run ((x, y) : rest) game =
  case playMove game.turn (Play (Coord x y)) game of
    Left  reason -> giveUp ("refused " <> show (x, y, reason))
    Right game'  -> run rest game'

prop_startsEmptyWithBlackToPlay :: Property
prop_startsEmptyWithBlackToPlay = withTests 1 . property $ do
  let game = newGame 19
  game.turn === Black
  boardSize game === 19
  finished game === False

prop_refusesAMoveByTheWrongPlayer :: Property
prop_refusesAMoveByTheWrongPlayer = withTests 1 . property $ do
  let game = newGame 9
  case playMove White (Play (Coord 3 3)) game of
    Right _ -> giveUp "White moved before Black"
    Left  _ -> pure ()

prop_countsCaptures :: Property
prop_countsCaptures = withTests 1 . property $ do
  game <- run
    [ (3, 3)  -- B, the stone that will be taken
    , (2, 3)  -- W
    , (8, 8)  -- B, somewhere else
    , (4, 3)  -- W
    , (8, 7)  -- B
    , (3, 2)  -- W
    , (8, 6)  -- B
    , (3, 4)  -- W, closes it
    ]
    (newGame 9)
  game.captures.white === 1
  game.captures.black === 0
  stoneAt game.board (Coord 3 3) === Nothing

prop_refusesTakingAKoBack :: Property
prop_refusesTakingAKoBack = withTests 1 . property $ do
  -- Black plays into the ko shape and takes one white stone. The point
  -- that stone came off is then closed to White for one move.
  game <- run
    [ (1, 0)  -- B
    , (2, 0)  -- W
    , (0, 1)  -- B
    , (3, 1)  -- W
    , (1, 2)  -- B
    , (2, 2)  -- W
    , (2, 1)  -- B takes nothing yet
    ]
    (newGame 9)
  -- White takes the black stone at (2, 1) by playing (1, 1).
  afterWhite <- case playMove White (Play (Coord 1 1)) game of
    Left  reason -> giveUp ("refused: " <> show reason)
    Right g      -> pure g
  afterWhite.koPoint === Just (Coord 2 1)
  playMove Black (Play (Coord 2 1)) afterWhite === Left KoRepeat
  -- Playing elsewhere opens the point again.
  elsewhere <- case playMove Black (Play (Coord 8 8)) afterWhite of
    Left  reason -> giveUp ("refused: " <> show reason)
    Right g      -> pure g
  elsewhere.koPoint === Nothing

prop_twoPassesEndTheGame :: Property
prop_twoPassesEndTheGame = withTests 1 . property $ do
  let game = newGame 9
  once <- case playMove Black Pass game of
    Left  reason -> giveUp (show reason)
    Right g      -> pure g
  finished once === False
  twice <- case playMove White Pass once of
    Left  reason -> giveUp (show reason)
    Right g      -> pure g
  finished twice === True

prop_aPassBetweenMovesDoesNotEndTheGame :: Property
prop_aPassBetweenMovesDoesNotEndTheGame = withTests 1 . property $ do
  game   <- run [(3, 3)] (newGame 9)
  passed <- case playMove White Pass game of
    Left  reason -> giveUp (show reason)
    Right g      -> pure g
  played <- run [(4, 4)] passed
  played.passes === 0
  finished played === False

prop_resignationEndsTheGame :: Property
prop_resignationEndsTheGame = withTests 1 . property $ do
  case playMove Black Resign (newGame 9) of
    Left  reason -> giveUp (show reason)
    Right game   -> do
      finished game === True
      winnerByResignation game === Just White

prop_undoPutsTheStonesBack :: Property
prop_undoPutsTheStonesBack = withTests 1 . property $ do
  -- Black's stone is taken, then the capture is taken back, and the
  -- stone has to be on the board again with the count back to zero.
  game <- run
    [ (3, 3)  -- B
    , (2, 3)  -- W
    , (8, 8)  -- B
    , (4, 3)  -- W
    , (8, 7)  -- B
    , (3, 2)  -- W
    , (8, 6)  -- B
    , (3, 4)  -- W, takes the stone at (3, 3)
    ]
    (newGame 9)
  case undoMove game of
    Nothing     -> giveUp "nothing to take back"
    Just before -> do
      stoneAt before.board (Coord 3 3) === Just Black
      before.captures.white === 0
      before.turn === White

prop_undoOfANewGameIsNothing :: Property
prop_undoOfANewGameIsNothing = withTests 1 . property $ do
  (undoMove (newGame 9) == Nothing) === True

prop_everyMoveIsWrittenDown :: Property
prop_everyMoveIsWrittenDown = withTests 1 . property $ do
  game <- run [(3, 3)] (newGame 9)
  passed <- case playMove White Pass game of
    Left  reason -> giveUp (show reason)
    Right passed -> pure passed
  played <- run [(4, 4)] passed
  resigned <- case playMove White Resign played of
    Left  reason   -> giveUp (show reason)
    Right resigned -> pure resigned
  resigned.moves
    === [Resign, Play (Coord 4 4), Pass, Play (Coord 3 3)]

prop_aPassCanBeTakenBack :: Property
prop_aPassCanBeTakenBack = withTests 1 . property $ do
  game <- run [(3, 3)] (newGame 9)
  passed <- case playMove White Pass game of
    Left  reason -> giveUp (show reason)
    Right passed -> pure passed
  passed.passes === 1
  case undoMove passed of
    Nothing   -> giveUp "nothing to take back"
    Just back -> do
      back.passes === 0
      back.turn === White
      back.moves === [Play (Coord 3 3)]

prop_aFinishedGameTakesNoMoreMoves :: Property
prop_aFinishedGameTakesNoMoreMoves = withTests 1 . property $ do
  let game = newGame 9
  over <- case playMove Black Pass game >>= playMove White Pass of
    Left  reason -> giveUp (show reason)
    Right over   -> pure over
  finished over === True
  playMove Black (Play (Coord 3 3)) over === Left GameOver
  playMove Black Pass over === Left GameOver
  legal Black (Coord 3 3) over === Left GameOver

prop_askingWhetherAMoveIsLegalAnswersTheSame :: Property
prop_askingWhetherAMoveIsLegalAnswersTheSame = withTests 1 . property $ do
  let game = newGame 9
  legal Black (Coord 3 3) game === Right ()
  legal White (Coord 3 3) game === Left WrongPlayer
  legal Black (Coord 9 0) game === Left OffBoard
  played <- run [(3, 3)] game
  legal White (Coord 3 3) played === Left Occupied

prop_twoGamesThatStandTheSameAreTheSame :: Property
prop_twoGamesThatStandTheSameAreTheSame = withTests 1 . property $ do
  -- The same position, reached two ways: one game never moved, and the
  -- other moved and took it back. What is behind them differs and they
  -- are still the same game to play on from.
  played <- run [(3, 3)] (newGame 9)
  case undoMove played of
    Nothing   -> giveUp "nothing to take back"
    Just back -> do
      assert (back == newGame 9)
      assert (played /= newGame 9)
      assert (newGame 9 /= newGame 13)

prop_aGamePrintsWhereItStandsAndNotHowItGotThere :: Property
prop_aGamePrintsWhereItStandsAndNotHowItGotThere =
  withTests 1 . property $ do
    played <- run [(3, 3)] (newGame 9)
    let printed = show played
    assert ("turn = White" `isInfixOf` printed)
    assert ("passes = 0" `isInfixOf` printed)
    assert ("resigned = Nothing" `isInfixOf` printed)
    -- The history is as long as the game, so it is left out.
    assert (not ("history" `isInfixOf` printed))
    assert (not ("gameHistory" `isInfixOf` printed))

prop_theWinnerOfAResignationIsTheOtherPlayer :: Property
prop_theWinnerOfAResignationIsTheOtherPlayer = withTests 1 . property $ do
  winnerByResignation (newGame 9) === Nothing
  case playMove Black Resign (newGame 9) of
    Left  reason -> giveUp (show reason)
    Right game   -> winnerByResignation game === Just White
  played <- run [(3, 3)] (newGame 9)
  case playMove White Resign played of
    Left  reason -> giveUp (show reason)
    Right game   -> winnerByResignation game === Just Black

tests :: Group
tests = $$(discover)