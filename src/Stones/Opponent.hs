-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

-- | The other side of a game, whatever is on it.
--
-- GNU Go answers when it is asked and says nothing otherwise. A game
-- on a server answers when it likes, and says things nobody asked
-- about: the other player moved, or wrote something, or went away. A
-- seam shaped as a question and an answer fits the first and not the
-- second.
--
-- So this one has two halves that do not line up. What this program
-- does is 'Told' to the other side, one thing at a time. What arrives
-- is 'Heard', whether anybody asked for it or not. An opponent that
-- only answers questions, like GNU Go, does its answering by putting
-- the answer on the stream, so the program above cannot tell the two
-- kinds apart and does not have to.
module Stones.Opponent
  ( Opponent(..)
  , Opponents(..)
  , Told(..)
  , Heard(..)
  , Opening(..)
  )
where

import           Data.Text                      ( Text )
import           Pipes                          ( Producer )

import           Go.Types
import           Stones.Setup                   ( Setup )

-- | The other side of one game.
data Opponent = Opponent
  { name  :: Text
    -- ^ What to call it in the window.
  , tell  :: Told -> IO (Either Text ())
    -- ^ Say one thing to it. The answer says whether it was said, and
    -- not what came of it: what comes of it arrives on 'hear', because
    -- with a server that is where it would arrive anyway.
  , hear  :: Producer Heard IO ()
    -- ^ Everything that arrives. This runs for as long as the game is
    -- open, and a game that ends is a producer that ends.
  , leave :: IO ()
    -- ^ Let it go. This does not fail: there is nothing the window
    -- would do about it.
  }

-- | Where a game gets an opponent, and where it gives one back.
data Opponents = Opponents
  { open  :: Setup -> IO (Either Text Opponent)
    -- ^ An opponent for a game set up like this.
  , close :: Opponent -> IO ()
    -- ^ Let one go, when the game it was playing has closed.
  }

-- | What this program does, which the other side is told about.
--
-- The rules live in this program rather than in the opponent, so what
-- follows a move is something this program knows and the other side
-- may not. That is why a move and the turn that follows it are two
-- things rather than one: an opponent is told what happened, and then
-- told whether anybody is to move next.
data Told
  = Moved Move
    -- ^ A move was made on this board.
  | TookBack Int
    -- ^ This many moves came off this board.
  | Asked
    -- ^ It is the other side's turn.
  | Resigned
    -- ^ The player gave the game up.
  | Counting
    -- ^ Nobody is to move, because the game has ended. What is the
    -- score?
  | Said Text
    -- ^ Something to say to whoever is listening.
  deriving (Eq, Show)

-- | The position a game starts from, which is not always an empty
-- board.
--
-- A handicap game starts with Black's stones already down and White to
-- play. A game being rejoined starts wherever it had got to. Both are
-- this, and the side that knows is the other one.
data Opening = Opening
  { width  :: Int
  , stones :: [(Color, Coord)]
    -- ^ What is already on the board.
  , opens  :: Color
    -- ^ Who plays next from here.
  , yours  :: Color
    -- ^ The colour the player at this window has in this game, which
    -- is something only the other side knows when it did the pairing.
  }
  deriving (Eq, Show)

-- | What arrives from the other side.
data Heard
  = Began Opening
    -- ^ This is the game, and this is where it stands. It arrives
    -- before anything else and can arrive again.
  | Played Color Move
    -- ^ This was played, by this colour. The colour is said rather
    -- than assumed, because a server says it and is the one that
    -- knows.
  | Spoke Text Text
    -- ^ Who said it, and what they said.
  | Undid Int
    -- ^ This many moves came off over there.
  | Counted Text
    -- ^ What the game came to.
  | Lost Text
    -- ^ The other side has gone, and this is why. Nothing follows it.
  deriving (Eq, Show)
