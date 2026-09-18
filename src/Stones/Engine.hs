-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedStrings     #-}

-- | What the program needs from an opponent.
--
-- Everything the application does to an opponent is one of the seven
-- actions below, so an opponent is a record of them rather than a
-- class. GNU Go is one, in "Stones.Engine.GnuGo". A game on a server
-- is the next one, and it fits the same shape: a move is sent, a move
-- comes back, and the board says what the score was.
--
-- Every action that can fail answers with 'Either', and the left side
-- is what to show the player. An opponent that is a process on this
-- machine and an opponent that is a connection over the network fail
-- in different ways, and the application does the same thing with
-- both: it stops asking and it says what happened.
module Stones.Engine
  ( Engine(..)
  , Opponents(..)
  , Strength(..)
  , strengths
  , describeStrength
  )
where

import           Data.Text                      ( Text )

import           Go.Types

-- | Where a program gets its opponents and where it gives them back.
--
-- A window with several games in it needs an opponent for each, so
-- something has to make them one at a time rather than hand over the
-- one there is. Whatever makes them also knows how to let one go, and
-- knows what is still running when the window closes, so the two
-- belong together.
data Opponents = Opponents
  { open  :: Int -> Strength -> IO (Either Text Engine)
    -- ^ An opponent of its own, for a game on a board this wide,
    -- trying this hard.
  , close :: Engine -> IO ()
    -- ^ Let one go, when the game it was playing has closed.
  }

-- | How hard an opponent is asked to try.
--
-- Three of them. A program that plays this well has ten steps to offer
-- and a menu of ten numbers is a menu nobody reads, so what the player
-- picks from is three words and what they mean is the program's
-- business.
data Strength
  = Gentle
  | Fair
  | Fierce
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Every strength, easiest first, which is the order a menu shows
-- them in.
strengths :: [Strength]
strengths = [minBound .. maxBound]

-- | What to call one in the window.
describeStrength :: Strength -> Text
describeStrength Gentle = "Gentle"
describeStrength Fair   = "Fair"
describeStrength Fierce = "Fierce"

-- | An opponent, and the handful of things that can be asked of one.
data Engine = Engine
  { name    :: Text
    -- ^ What to call this opponent in the window.
  , newGame :: Int -> IO (Either Text ())
    -- ^ Clear the board and play on one of this width from now on.
  , notify  :: Color -> Move -> IO (Either Text ())
    -- ^ Tell the opponent about a move somebody else made.
  , genMove :: Color -> IO (Either Text Move)
    -- ^ Ask the opponent for a move of its own, which it also plays.
  , undo    :: Int -> IO (Either Text ())
    -- ^ Take this many moves back off the opponent's board.
  , score   :: IO (Either Text Text)
    -- ^ What the opponent makes the score, once the game has ended.
  , close   :: IO ()
    -- ^ Let the opponent go. This does not fail: there is nothing the
    -- application would do about it.
  }
