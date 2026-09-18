{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | The rules that need more than one position: ko, passing, and
-- taking a move back.
module GameTest
  ( tests
  )
where

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
  case playMove (gameTurn game) (Play (Coord x y)) game of
    Left  reason -> giveUp ("refused " <> show (x, y, reason))
    Right game'  -> run rest game'

prop_startsEmptyWithBlackToPlay :: Property
prop_startsEmptyWithBlackToPlay = withTests 1 . property $ do
  let game = newGame 19
  gameTurn game === Black
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
  whiteCaptured (gameCaptures game) === 1
  blackCaptured (gameCaptures game) === 0
  stoneAt (gameBoard game) (Coord 3 3) === Nothing

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
  gameKoPoint afterWhite === Just (Coord 2 1)
  playMove Black (Play (Coord 2 1)) afterWhite === Left KoRepeat
  -- Playing elsewhere opens the point again.
  elsewhere <- case playMove Black (Play (Coord 8 8)) afterWhite of
    Left  reason -> giveUp ("refused: " <> show reason)
    Right g      -> pure g
  gameKoPoint elsewhere === Nothing

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
  gamePasses played === 0
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
      stoneAt (gameBoard before) (Coord 3 3) === Just Black
      whiteCaptured (gameCaptures before) === 0
      gameTurn before === White

prop_undoOfANewGameIsNothing :: Property
prop_undoOfANewGameIsNothing = withTests 1 . property $ do
  (undoMove (newGame 9) == Nothing) === True

tests :: Group
tests = $$(discover)
