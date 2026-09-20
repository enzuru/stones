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
  , choosing
  , Told(..)
  , Heard(..)
  , Opening(..)
  , Clock(..)
  , Time(..)
  , ticking
  , describeTime
  )
where

import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Pipes                          ( Producer )

import           Go.Types
import           Stones.Setup                   ( Against(..)
                                                , Setup(..)
                                                )

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
  , enter :: IO (Either Text Text)
    -- ^ Sign in wherever the games are, answering with what to call
    -- the player. Only one of the two places asks for it, and the
    -- window only asks when that is the place chosen.
  }

-- | One place to get a game from, made of two, picked by where the
-- game says it is played.
--
-- Letting one go is the same either way: an opponent knows how to
-- leave, and neither place keeps anything about it that outliving the
-- leaving would matter to.
choosing :: Opponents -> Opponents -> Opponents
choosing computer online = Opponents
  { open  = \setup -> case setup.against of
              Computer -> computer.open setup
              Online   -> online.open setup
  , close = (.leave)
  , enter = online.enter
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
  | Marking [Coord] Bool
    -- ^ These stones are dead, or are alive after all. At the end of a
    -- game the two players agree which stones would have been taken if
    -- play had gone on, because nobody plays those moves out.
  | Accepting [Coord]
    -- ^ This is the set of dead stones, as far as this player is
    -- concerned. The game is counted once both sides have said so
    -- about the same set.
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

-- | How long each player has left.
data Clock = Clock
  { black   :: Time
  , white   :: Time
  , running :: Maybe Color
    -- ^ Whose is going down. Nobody's before the game starts and after
    -- it ends.
  }
  deriving (Eq, Show)

-- | One player's clock.
--
-- A game is usually counted in two parts: the time to think in, and
-- then a number of shorter periods, each of which is given back if the
-- player moves inside it. What is left of whichever is running now is
-- the first of these, and how many are still to come is the second.
data Time = Time
  { seconds :: Int
  , periods :: Int
    -- ^ How many short periods are still to come, or none.
  , each    :: Int
    -- ^ How long one of those is.
  }
  deriving (Eq, Show)

-- | A second going by.
--
-- The server says where a clock is when something happens, and says
-- nothing in between, so the seconds in between are counted here. What
-- the server says next is taken over this, because the server is the
-- one that decides.
ticking :: Clock -> Clock
ticking clock = Clock { black   = down Black clock.black
                      , white   = down White clock.white
                      , running = clock.running
                      }
 where
  down side time
    | clock.running /= Just side = time
    | time.seconds > 0           = time { seconds = time.seconds - 1 }
    | time.periods > 0           = time { seconds = max 0 (time.each - 1)
                                        , periods = time.periods - 1
                                        }
    | otherwise                  = time

-- | A clock as a window shows one.
describeTime :: Time -> Text
describeTime time = clock <> extra
 where
  clock =
    Text.pack (show minutes) <> ":" <> Text.justifyRight 2 '0' (Text.pack (show rest))
  (minutes, rest) = time.seconds `divMod` 60
  extra | time.periods > 0 = " (\215" <> Text.pack (show time.periods) <> ")"
        | otherwise        = ""

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
  | Stopped
    -- ^ Nobody moves again. What is left is agreeing which stones are
    -- dead, and the game is counted from there.
  | Removing [Coord]
    -- ^ The stones marked dead, as they now stand. The whole set every
    -- time, rather than what changed, so a window that missed one is
    -- not left showing a board nobody else can see.
  | Ticked Clock
    -- ^ Where the clocks are. It arrives when something happens and
    -- not in between.
  | Counted Text
    -- ^ What the game came to.
  | Lost Text
    -- ^ The other side has gone, and this is why. Nothing follows it.
  deriving (Eq, Show)
