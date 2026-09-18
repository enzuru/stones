-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE NoFieldSelectors      #-}
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
  { board    :: !Board
    -- ^ The position now.
  , turn     :: !Color
    -- ^ Who plays next.
  , captures :: !Captures
    -- ^ How many stones each player has taken.
  , koPoint  :: !(Maybe Coord)
    -- ^ The point that is closed for one move, if a ko was just taken.
  , last     :: !(Maybe Coord)
    -- ^ Where the last stone went, which the board marks.
  , moves    :: ![Move]
    -- ^ Every move so far, the newest first.
  , passes   :: !Int
    -- ^ How many passes in a row have just been made.
  , resigned :: !(Maybe Color)
    -- ^ Who resigned, if anybody did.
  , history  :: ![Game]
    -- ^ The game before each move, the newest first.
  }

-- | Two games are equal when they stand the same. The history behind
-- them is left out: it is as long as the game, so comparing it would
-- cost more the longer the game ran, and two games that stand the same
-- play the same from here whatever was played to get there.
instance Eq Game where
  one == other =
    one.board
      ==     other.board
      &&     one.turn
      ==     other.turn
      &&     one.captures
      ==     other.captures
      &&     one.koPoint
      ==     other.koPoint
      &&     one.last
      ==     other.last
      &&     one.passes
      ==     other.passes
      &&     one.resigned
      ==     other.resigned

-- | The position and what is left to decide about it, without the
-- history, for the same reason.
instance Show Game where
  show game =
    "Game { board = "
      <> show game.board
      <> ", turn = "
      <> show game.turn
      <> ", captures = "
      <> show game.captures
      <> ", ko = "
      <> show game.koPoint
      <> ", passes = "
      <> show game.passes
      <> ", resigned = "
      <> show game.resigned
      <> " }"

-- | A game on an empty board of this width, with Black to play.
newGame :: Int -> Game
newGame n = Game { board    = emptyBoard n
                 , turn     = Black
                 , captures = noCaptures
                 , koPoint  = Nothing
                 , last     = Nothing
                 , moves    = []
                 , passes   = 0
                 , resigned = Nothing
                 , history  = []
                 }

-- | The width of the board this game is played on.
boardSize :: Game -> Int
boardSize game = game.board.size

-- | Is the game over? Two passes in a row end it, and so does a
-- resignation.
finished :: Game -> Bool
finished game = game.passes >= 2 || game.resigned /= Nothing

-- | Who won, if the game ended in a resignation.
winnerByResignation :: Game -> Maybe Color
winnerByResignation game = opposite <$> game.resigned

-- | Can this player put a stone on this point? The answer is the same
-- one 'playMove' gives, without the new game.
legal :: Color -> Coord -> Game -> Either Illegal ()
legal color coord game
  | finished game              = Left GameOver
  | color /= game.turn         = Left WrongPlayer
  | game.koPoint == Just coord = Left KoRepeat
  | otherwise                  = () <$ place color coord game.board

-- | Play a move for the player whose turn it is.
--
-- A move by the other player is refused the same way an illegal one
-- is, so a mix-up between the two halves of the program shows up here
-- rather than as a board that quietly disagrees with the engine.
playMove :: Color -> Move -> Game -> Either Illegal Game
playMove color move game
  | finished game      = Left GameOver
  | color /= game.turn = Left WrongPlayer
  | otherwise          = case move of
  Pass   -> Right (record game) { turn     = opposite color
                                , koPoint  = Nothing
                                , last     = Nothing
                                , moves    = Pass : game.moves
                                , passes   = game.passes + 1
                                }
  Resign -> Right (record game) { turn     = opposite color
                                , koPoint  = Nothing
                                , moves    = Resign : game.moves
                                , resigned = Just color
                                }
  Play coord -> do
    () <- legal color coord game
    Placement board captured <- place color coord game.board
    Right (record game)
      { board    = board
      , turn     = opposite color
      , captures = addCapture color (Set.size captured) game.captures
      , koPoint  = koAfter board coord captured
      , last     = Just coord
      , moves    = Play coord : game.moves
      , passes   = 0
      }
 where
  record before = before { history = before : before.history }

-- | The point a ko closes for one move.
--
-- This is the simple ko rule, which is the one GNU Go plays by
-- default. A ko is the one shape where taking straight back would
-- repeat the position: a single stone takes a single stone, and the
-- stone that took it is alone with one liberty. Anything else, a
-- capture of two or a capture by a group, cannot be taken back into
-- the same position, so nothing is closed.
koAfter :: Board -> Coord -> Set.Set Coord -> Maybe Coord
koAfter board coord captured = case Set.toList captured of
  [taken] | Set.size placed == 1 && Set.size (liberties board placed) == 1 ->
    Just taken
  _ -> Nothing
  where placed = group board coord

-- | Take the last move back, if there is one.
undoMove :: Game -> Maybe Game
undoMove game = case game.history of
  before : _ -> Just before
  []         -> Nothing
