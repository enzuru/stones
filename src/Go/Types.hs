-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The values a game of Go is made of: the two colours, a point on
-- the board, and what a player does on a turn.
module Go.Types
  ( Color(..)
  , opposite
  , Coord(..)
  , Move(..)
  , Captures(..)
  , noCaptures
  , capturedBy
  , addCapture
  , Illegal(..)
  , describeIllegal
  )
where

import           Data.Text                      ( Text )

-- | Which player a stone belongs to.
data Color
  = Black
  | White
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | The other player.
opposite :: Color -> Color
opposite Black = White
opposite White = Black

-- | A point on the board, counted from the top left corner, so that
-- @Coord 0 0@ is the point the drawing code puts first. Both numbers
-- run from zero to one less than the board size.
data Coord = Coord
  { coordX :: !Int
  , coordY :: !Int
  }
  deriving (Eq, Ord, Show)

-- | What a player does on a turn.
data Move
  = Play !Coord
  | Pass
  | Resign
  deriving (Eq, Show)

-- | How many stones each player has taken off the board.
data Captures = Captures
  { blackCaptured :: !Int
    -- ^ White stones that Black has taken.
  , whiteCaptured :: !Int
    -- ^ Black stones that White has taken.
  }
  deriving (Eq, Show)

-- | Why a move cannot be played.
--
-- The first four are what a single position can decide, and
-- "Go.Board" answers with those. The last two need the game around
-- the position, and "Go.Game" adds them.
data Illegal
  = OffBoard
    -- ^ The point named is not on this board.
  | Occupied
    -- ^ There is already a stone there.
  | Suicide
    -- ^ The stone, and the group it joins, would have no liberties.
  | KoRepeat
    -- ^ The move would take back a ko straight away.
  | WrongPlayer
    -- ^ It is the other player's turn.
  | GameOver
    -- ^ The game has already ended.
  deriving (Eq, Show)

-- | What to tell the player about a move that was refused.
describeIllegal :: Illegal -> Text
describeIllegal = \case
  OffBoard    -> "That point is not on the board."
  Occupied    -> "There is already a stone there."
  Suicide     -> "That move would leave the group with no liberties."
  KoRepeat    -> "Ko: that point is closed for one move."
  WrongPlayer -> "It is the other player's turn."
  GameOver    -> "The game has ended."

-- | Nobody has taken anything yet.
noCaptures :: Captures
noCaptures = Captures 0 0

-- | The number of stones this player has taken.
capturedBy :: Color -> Captures -> Int
capturedBy Black = blackCaptured
capturedBy White = whiteCaptured

-- | Add stones to this player's total.
addCapture :: Color -> Int -> Captures -> Captures
addCapture _     0 captures = captures
addCapture Black n captures =
  captures { blackCaptured = blackCaptured captures + n }
addCapture White n captures =
  captures { whiteCaptured = whiteCaptured captures + n }
