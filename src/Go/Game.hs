-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE LambdaCase #-}

-- | A game in progress: the board, whose turn it is, what has been
-- taken, and the rules that need more than one position to decide.
module Go.Game
  ( Game(..)
  , newGame
  , legal
  , playMove
  , undoMove
  , finished
  , winnerByResignation
  , boardSize
  )
where

import qualified Data.Set                      as Set

import           Go.Board
import           Go.Types

-- | Everything about a game that the rules decide.
--
-- The earlier positions are kept whole rather than as a list of moves,
-- because taking a move back has to put the captured stones back too,
-- and the position before the move is the shortest way to say that.
data Game = Game
  { gameBoard    :: !Board
    -- ^ The position now.
  , gameTurn     :: !Color
    -- ^ Who plays next.
  , gameCaptures :: !Captures
    -- ^ How many stones each player has taken.
  , gameKoPoint  :: !(Maybe Coord)
    -- ^ The point that is closed for one move, if a ko was just taken.
  , gameLast     :: !(Maybe Coord)
    -- ^ Where the last stone went, which the board marks.
  , gameMoves    :: ![Move]
    -- ^ Every move so far, the newest first.
  , gamePasses   :: !Int
    -- ^ How many passes in a row have just been made.
  , gameResigned :: !(Maybe Color)
    -- ^ Who resigned, if anybody did.
  , gameHistory  :: ![Game]
    -- ^ The game before each move, the newest first.
  }

-- | Two games are equal when they stand the same. The history behind
-- them is left out: it is as long as the game, so comparing it would
-- cost more the longer the game ran, and two games that stand the same
-- play the same from here whatever was played to get there.
instance Eq Game where
  one == other =
    gameBoard one
      ==     gameBoard other
      &&     gameTurn one
      ==     gameTurn other
      &&     gameCaptures one
      ==     gameCaptures other
      &&     gameKoPoint one
      ==     gameKoPoint other
      &&     gameLast one
      ==     gameLast other
      &&     gamePasses one
      ==     gamePasses other
      &&     gameResigned one
      ==     gameResigned other

-- | The position and what is left to decide about it, without the
-- history, for the same reason.
instance Show Game where
  show game =
    "Game { board = "
      <> show (gameBoard game)
      <> ", turn = "
      <> show (gameTurn game)
      <> ", captures = "
      <> show (gameCaptures game)
      <> ", ko = "
      <> show (gameKoPoint game)
      <> ", passes = "
      <> show (gamePasses game)
      <> ", resigned = "
      <> show (gameResigned game)
      <> " }"

-- | A game on an empty board of this width, with Black to play.
newGame :: Int -> Game
newGame n = Game { gameBoard    = emptyBoard n
                 , gameTurn     = Black
                 , gameCaptures = noCaptures
                 , gameKoPoint  = Nothing
                 , gameLast     = Nothing
                 , gameMoves    = []
                 , gamePasses   = 0
                 , gameResigned = Nothing
                 , gameHistory  = []
                 }

-- | The width of the board this game is played on.
boardSize :: Game -> Int
boardSize = size . gameBoard

-- | Is the game over? Two passes in a row end it, and so does a
-- resignation.
finished :: Game -> Bool
finished game = gamePasses game >= 2 || gameResigned game /= Nothing

-- | Who won, if the game ended in a resignation.
winnerByResignation :: Game -> Maybe Color
winnerByResignation = fmap opposite . gameResigned

-- | Can this player put a stone on this point? The answer is the same
-- one 'playMove' gives, without the new game.
legal :: Color -> Coord -> Game -> Either Illegal ()
legal color coord game
  | finished game                  = Left GameOver
  | color /= gameTurn game         = Left WrongPlayer
  | gameKoPoint game == Just coord = Left KoRepeat
  | otherwise                      = () <$ place color coord (gameBoard game)

-- | Play a move for the player whose turn it is.
--
-- A move by the other player is refused the same way an illegal one
-- is, so a mix-up between the two halves of the program shows up here
-- rather than as a board that quietly disagrees with the engine.
playMove :: Color -> Move -> Game -> Either Illegal Game
playMove color move game
  | finished game          = Left GameOver
  | color /= gameTurn game = Left WrongPlayer
  | otherwise              = case move of
  Pass   -> Right (record game) { gameTurn     = opposite color
                                , gameKoPoint  = Nothing
                                , gameLast     = Nothing
                                , gameMoves    = Pass : gameMoves game
                                , gamePasses   = gamePasses game + 1
                                }
  Resign -> Right (record game) { gameTurn     = opposite color
                                , gameKoPoint  = Nothing
                                , gameMoves    = Resign : gameMoves game
                                , gameResigned = Just color
                                }
  Play coord -> do
    () <- legal color coord game
    Placement board captured <- place color coord (gameBoard game)
    Right (record game)
      { gameBoard    = board
      , gameTurn     = opposite color
      , gameCaptures = addCapture color (Set.size captured) (gameCaptures game)
      , gameKoPoint  = koPoint board coord captured
      , gameLast     = Just coord
      , gameMoves    = Play coord : gameMoves game
      , gamePasses   = 0
      }
 where
  record before = before { gameHistory = before : gameHistory before }

-- | The point a ko closes for one move.
--
-- This is the simple ko rule, which is the one GNU Go plays by
-- default. A ko is the one shape where taking straight back would
-- repeat the position: a single stone takes a single stone, and the
-- stone that took it is alone with one liberty. Anything else, a
-- capture of two or a capture by a group, cannot be taken back into
-- the same position, so nothing is closed.
koPoint :: Board -> Coord -> Set.Set Coord -> Maybe Coord
koPoint board coord captured = case Set.toList captured of
  [taken] | Set.size placed == 1 && Set.size (liberties board placed) == 1 ->
    Just taken
  _ -> Nothing
  where placed = group board coord

-- | Take the last move back, if there is one.
undoMove :: Game -> Maybe Game
undoMove game = case gameHistory game of
  before : _ -> Just before
  []         -> Nothing
